(ns hoatzin.coretext
  "Text shaping, wrapping and rasterization through CoreText (macOS only).

  Lines are drawn into a CoreGraphics bitmap as premultiplied RGBA, in their
  final colour: tinting white text on the GPU would also tint colour glyphs
  such as emoji.

  CoreText indexes strings in UTF-16 code units, and every index in this
  namespace is one. Converting to and from Jolt's code-point indices is the
  caller's job (see hoatzin.layout)."
  (:require [jolt.ffi :as ffi]))

;; ---------------------------------------------------------------- bindings

;; CF object refs travel as :iptr, not :pointer: CoreFoundation hands back
;; tagged pointers (top bit set) for small strings and numbers, which an
;; unsigned :pointer reads as a BigInt that can't be passed back to C.
;;
;; :double arguments must be given floating-point values; an integer is
;; rejected at the call ("invalid foreign-procedure argument").

(def ^:private UTF8 0x08000100)                    ; kCFStringEncodingUTF8
(def ^:private ALPHA-PREMULTIPLIED-LAST 1)         ; kCGImageAlphaPremultipliedLast
(def ^:private BYTE-ORDER-32-BIG (bit-shift-left 4 12)) ; kCGBitmapByteOrder32Big

(def ^:private cfrange (ffi/layout [:struct [[:location :long] [:length :long]]]))
(def ^:private cgpoint (ffi/layout [:struct [[:x :double] [:y :double]]]))

(ffi/defcfn ^:private cf-release "CFRelease" [:iptr] :void)
(ffi/defcfn ^:private cf-string-create "CFStringCreateWithCString"
  [:pointer :string :uint] :iptr)
(ffi/defcfn ^:private cf-string-length "CFStringGetLength" [:iptr] :long)
(ffi/defcfn ^:private cf-string-get-cstring "CFStringGetCString"
  [:iptr :pointer :long :uint] :bool)
(ffi/defcfn ^:private cf-string-composed-range "CFStringGetRangeOfComposedCharactersAtIndex"
  [:iptr :long] [:by-value [:struct [[:location :long] [:length :long]]]])
(ffi/defcfn ^:private cf-dictionary-create "CFDictionaryCreate"
  [:pointer :pointer :pointer :long :pointer :pointer] :iptr)
(ffi/defcfn ^:private cf-attributed-string-create "CFAttributedStringCreate"
  [:pointer :iptr :iptr] :iptr)

(ffi/defcfn ^:private ct-font-create-with-name "CTFontCreateWithName"
  [:iptr :double :pointer] :iptr)
(ffi/defcfn ^:private ct-font-copy-family-name "CTFontCopyFamilyName" [:iptr] :iptr)
(ffi/defcfn ^:private ct-font-ascent  "CTFontGetAscent"  [:iptr] :double)
(ffi/defcfn ^:private ct-font-descent "CTFontGetDescent" [:iptr] :double)
(ffi/defcfn ^:private ct-font-leading "CTFontGetLeading" [:iptr] :double)

(ffi/defcfn ^:private ct-typesetter-create "CTTypesetterCreateWithAttributedString"
  [:iptr] :iptr)
(ffi/defcfn ^:private ct-typesetter-suggest-line-break "CTTypesetterSuggestLineBreak"
  [:iptr :long :double] :long)
(ffi/defcfn ^:private ct-typesetter-create-line "CTTypesetterCreateLine"
  [:iptr [:by-value [:struct [[:location :long] [:length :long]]]]] :iptr)

(ffi/defcfn ^:private ct-line-typographic-bounds "CTLineGetTypographicBounds"
  [:iptr :pointer :pointer :pointer] :double)
(ffi/defcfn ^:private ct-line-offset-for-index "CTLineGetOffsetForStringIndex"
  [:iptr :long :pointer] :double)
(ffi/defcfn ^:private ct-line-index-for-position "CTLineGetStringIndexForPosition"
  [:iptr [:by-value [:struct [[:x :double] [:y :double]]]]] :long)
(ffi/defcfn ^:private ct-line-draw "CTLineDraw" [:iptr :pointer] :void)

(ffi/defcfn ^:private cg-color-space-create "CGColorSpaceCreateWithName" [:iptr] :pointer)
(ffi/defcfn ^:private cg-color-space-release "CGColorSpaceRelease" [:pointer] :void)
(ffi/defcfn ^:private cg-bitmap-context-create "CGBitmapContextCreate"
  [:pointer :size_t :size_t :size_t :size_t :pointer :uint] :pointer)
(ffi/defcfn ^:private cg-context-release "CGContextRelease" [:pointer] :void)
(ffi/defcfn ^:private cg-context-set-fill "CGContextSetRGBFillColor"
  [:pointer :double :double :double :double] :void)
(ffi/defcfn ^:private cg-context-set-text-position "CGContextSetTextPosition"
  [:pointer :double :double] :void)
(ffi/defcfn ^:private cg-context-set-should-smooth-fonts "CGContextSetShouldSmoothFonts"
  [:pointer :bool] :void)

(defn- symbol-address [sym]
  (or (ffi/find-symbol sym)
      (throw (ex-info (str "symbol not found: " sym) {:symbol sym}))))

(defn- global
  "The value of a global constant (a CFTypeRef) exported by a framework."
  [sym]
  (ffi/read (symbol-address sym) :iptr))

(def ^:private constants
  (delay {:font-attr          (global "kCTFontAttributeName")
          :fg-from-context    (global "kCTForegroundColorFromContextAttributeName")
          :cf-true            (global "kCFBooleanTrue")
          :srgb               (global "kCGColorSpaceSRGB")
          :dict-key-callbacks (symbol-address "kCFTypeDictionaryKeyCallBacks")
          :dict-val-callbacks (symbol-address "kCFTypeDictionaryValueCallBacks")}))

(defn- cf-string [s]
  (let [ref (cf-string-create ffi/null s UTF8)]
    (when (zero? ref) (throw (ex-info "CFStringCreateWithCString failed" {:s s})))
    ref))

(defn- cf-string->str [ref]
  (ffi/with-alloc [buf 1024]
    (when (cf-string-get-cstring ref buf 1024 UTF8)
      (ffi/ptr->string buf))))

(defn release
  "Release a CoreFoundation/CoreText object returned by this namespace."
  [ref]
  (when (and ref (not (zero? ref))) (cf-release ref)))

;; ---------------------------------------------------------------- fonts

(def default-family
  "A serif face that ships with every macOS install."
  "Georgia")

(defn- text-attributes
  "{kCTFontAttributeName font, kCTForegroundColorFromContextAttributeName true}"
  [font]
  (let [{:keys [font-attr fg-from-context cf-true dict-key-callbacks dict-val-callbacks]} @constants]
    (ffi/with-alloc [ks (* 2 (ffi/sizeof :pointer))]
      (ffi/with-alloc [vs (* 2 (ffi/sizeof :pointer))]
        (ffi/write ks :iptr font-attr 0)
        (ffi/write ks :iptr fg-from-context 8)
        (ffi/write vs :iptr font 0)
        (ffi/write vs :iptr cf-true 8)
        (cf-dictionary-create ffi/null ks vs 2 dict-key-callbacks dict-val-callbacks)))))

(defn font
  "Open a font by PostScript or family name at `size` pixels.

  Returns {:ref :attrs :family :ascent :descent :leading}; release it with
  `release-font`. CoreText substitutes a fallback for unknown names, and
  :family reports the face actually chosen."
  ([size] (font default-family size))
  ([family size]
   (let [name (cf-string family)
         f    (try (ct-font-create-with-name name (double size) ffi/null)
                   (finally (cf-release name)))]
     (when (zero? f) (throw (ex-info "CTFontCreateWithName failed" {:family family})))
     (let [fam (ct-font-copy-family-name f)]
       {:ref     f
        :attrs   (text-attributes f)
        :family  (try (cf-string->str fam) (finally (cf-release fam)))
        :ascent  (ct-font-ascent f)
        :descent (ct-font-descent f)
        :leading (ct-font-leading f)}))))

(defn release-font [{:keys [ref attrs]}]
  (release attrs)
  (release ref))

;; ---------------------------------------------------------------- paragraphs

(defn typeset
  "Shape `text` (one paragraph, no newlines) in `font` and break it into
  lines no wider than `width` pixels.

  Returns {:string cfstring :length n :lines [{:line ctline :start u :end u}]},
  with UTF-16 ranges. An empty paragraph has one line whose :line is nil.
  Release with `release-paragraph`."
  [font text width]
  (let [s (cf-string text)
        n (cf-string-length s)]
    (if (zero? n)
      {:string s :length 0 :lines [{:line nil :start 0 :end 0}]}
      (let [astr (cf-attributed-string-create ffi/null s (:attrs font))
            ts   (try (ct-typesetter-create astr) (finally (cf-release astr)))]
        (try
          (ffi/with-layout [r cfrange]
            (loop [start 0, lines []]
              (if (>= start n)
                {:string s :length n :lines lines}
                (let [cnt (max 1 (ct-typesetter-suggest-line-break ts start (double width)))]
                  (ffi/write-field r cfrange :location start)
                  (ffi/write-field r cfrange :length cnt)
                  (recur (+ start cnt)
                         (conj lines {:line  (ct-typesetter-create-line ts r)
                                      :start start
                                      :end   (+ start cnt)}))))))
          (finally (cf-release ts)))))))

(defn release-paragraph [{:keys [string lines]}]
  (doseq [{:keys [line]} lines] (release line))
  (release string))

(defn composed-range
  "The [start end) UTF-16 range of the user-perceived character (grapheme
  cluster) covering index `i` of `cfstring`."
  [cfstring i]
  (ffi/with-layout [r cfrange]
    (cf-string-composed-range r cfstring i)
    (let [loc (ffi/read-field r cfrange :location)]
      [loc (+ loc (ffi/read-field r cfrange :length))])))

;; ---------------------------------------------------------------- lines

(defn offset-for-index
  "Horizontal pixel offset of the caret before UTF-16 index `i` on `line`."
  [line i]
  (if line (ct-line-offset-for-index line i ffi/null) 0.0))

(defn index-for-position
  "The UTF-16 index nearest pixel offset `x` on `line`, or nil."
  [line x]
  (when line
    (ffi/with-layout [p cgpoint]
      (ffi/write-field p cgpoint :x (double x))
      (ffi/write-field p cgpoint :y 0.0)
      (let [i (ct-line-index-for-position line p)]
        (when-not (neg? i) i)))))

(defn rasterize-line
  "Draw `line` into a new RGBA bitmap in colour [r g b] (0-255), on
  transparent.

  Returns {:pixels ptr :width w :height h :pitch bytes :pad px :baseline px}:
  :baseline is the baseline's distance from the bitmap's top and :pad the
  blank margin left of the line's origin. Free :pixels with ffi/free."
  [line [r g b]]
  (ffi/with-out [pa :double]
    (ffi/with-out [pd :double]
      (let [advance (ct-line-typographic-bounds line pa pd ffi/null)
            ascent  (long (Math/ceil (ffi/read pa :double)))
            descent (long (Math/ceil (ffi/read pd :double)))
            pad     2                              ; room for glyph overhang
            w       (+ (long (Math/ceil advance)) (* 2 pad))
            h       (+ ascent descent (* 2 pad))
            pitch   (* 4 w)
            pixels  (ffi/alloc (* pitch h))
            cs      (cg-color-space-create (:srgb @constants))
            ctx     (cg-bitmap-context-create
                     pixels w h 8 pitch cs
                     (bit-or ALPHA-PREMULTIPLIED-LAST BYTE-ORDER-32-BIG))]
        (cg-color-space-release cs)
        (when (ffi/null? ctx)
          (ffi/free pixels)
          (throw (ex-info "CGBitmapContextCreate failed" {:w w :h h})))
        (try
          ;; LCD-style smoothing assumes an opaque backdrop; we have none.
          (cg-context-set-should-smooth-fonts ctx false)
          (cg-context-set-fill ctx (/ r 255.0) (/ g 255.0) (/ b 255.0) 1.0)
          ;; CG's origin is bottom-left. A whole-pixel baseline keeps glyphs crisp.
          (cg-context-set-text-position ctx (double pad) (double (+ pad descent)))
          (ct-line-draw line ctx)
          (finally (cg-context-release ctx)))
        {:pixels pixels :width w :height h :pitch pitch
         :pad pad :baseline (+ pad ascent)}))))
