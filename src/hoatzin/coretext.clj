(ns hoatzin.coretext
  "Text shaping, wrapping and rasterization through CoreText (macOS only).

  Lines are drawn into a CoreGraphics bitmap as premultiplied RGBA, in their
  final colour: tinting white text on the GPU would also tint colour glyphs
  such as emoji.

  Wrapping and drawing are separate. `wrap` finds a paragraph's line breaks
  and keeps nothing native; `make-line` sets one line's text as a CTLine,
  for measuring or drawing it, when that line is wanted.

  CoreText indexes strings in UTF-16 code units, and every index in this
  namespace is one. Converting to and from Jolt's code-point indices is the
  caller's job (see hoatzin.layout)."
  (:require [clojure.string :as str]
            [jolt.ffi :as ffi]))

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
(ffi/defcfn ^:private cf-string-char-at "CFStringGetCharacterAtIndex" [:iptr :long] :uint16)
(ffi/defcfn ^:private cf-string-create-substring "CFStringCreateWithSubstring"
  [:pointer :iptr [:by-value [:struct [[:location :long] [:length :long]]]]] :iptr)
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
(ffi/defcfn ^:private ct-line-create "CTLineCreateWithAttributedString" [:iptr] :iptr)

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

;; ---------------------------------------------------------------- wrapping

;; Lines are {:start u :end u}: UTF-16 ranges of their paragraph.

(def ^:dynamic *window*
  "How many UTF-16 units to typeset at once. A typesetter's line breaking
  costs in proportion to its whole string, on every line, so wrapping a
  long paragraph with one typesetter is quadratic: 100 KB takes most of a
  second. In windows of this size it takes milliseconds. Short paragraphs
  are typeset this many units together, which halves what typesetting
  each costs."
  2048)

(def ^:private margin
  "A window's lines must end this far before the window does (unless it
  ends the paragraph), so that what follows a line's break was in view
  when it was chosen."
  16)

(defn- typesetter
  "A typesetter over the UTF-16 range [ws, we) of `cfs`. Release it."
  [font cfs ws we]
  (ffi/with-layout [r cfrange]
    (ffi/write-field r cfrange :location ws)
    (ffi/write-field r cfrange :length (- we ws))
    (let [sub  (cf-string-create-substring ffi/null cfs r)
          astr (try (cf-attributed-string-create ffi/null sub (:attrs font))
                    (finally (cf-release sub)))]
      (try (ct-typesetter-create astr) (finally (cf-release astr))))))

(defn- suggest-break
  "Where typesetter `ts` would end a line that starts at its index `i`."
  [ts i width]
  (+ i (max 1 (ct-typesetter-suggest-line-break ts i (double width)))))

(defn- window-lines
  "Break the UTF-16 range [ws, we) of `cfs` into lines from `ws`, stopping
  before any line that reaches within `margin` of `we` (unless `we` is `n`,
  the paragraph's end)."
  [font cfs ws we n width]
  (let [ts (typesetter font cfs ws we)]
    (try
      (loop [start ws, lines []]
        (if (>= start we)
          lines
          (let [end (+ ws (suggest-break ts (- start ws) width))]
            (if (and (< we n) (> end (- we margin)))
              lines
              (recur end (conj lines {:start start :end end}))))))
      (finally (cf-release ts)))))

(defn- lines-from
  "The lines of paragraph `cfs`, `n` units long, from `from` (a line's
  start) to its end, a window at a time, the first `size` units long. With
  `stop?`, stops after the first line it accepts. Returns [lines stopped],
  `stopped` being that line, or nil."
  [font cfs from n width size stop?]
  (loop [ws from, acc [], size size]
    (if (>= ws n)
      [acc nil]
      (let [got (window-lines font cfs ws (min n (+ ws size)) n width)
            k   (when stop? (first (keep-indexed (fn [k ln] (when (stop? ln) k)) got)))]
        (cond
          (empty? got) (recur ws acc (* 2 size))   ; a line longer than the window
          k            [(into acc (subvec got 0 (inc k))) (nth got k)]
          :else        (recur (:end (peek got)) (into acc got) *window*))))))

(def ^:private empty-paragraph {:length 0 :lines [{:start 0 :end 0}]})

(defn- wrap-one
  "Line breaks for one paragraph, typeset a window at a time."
  [font text width]
  (let [cfs (cf-string text)
        n   (cf-string-length cfs)]
    (try
      (if (zero? n)
        empty-paragraph
        {:length n :lines (first (lines-from font cfs 0 n width *window* nil))})
      (finally (cf-release cfs)))))

(defn- wrap-together
  "Line breaks for short paragraphs, typeset as one string joined by
  newlines: CoreText breaks at each, ending the line before it with it."
  [font texts width]
  (let [cfs (cf-string (str/join "\n" texts))
        n   (cf-string-length cfs)]
    (try
      (if (zero? n)
        [empty-paragraph]
        (let [ts (typesetter font cfs 0 n)]
          (try
            ;; `p` is where the current paragraph starts in the joined string
            (loop [start 0, p 0, lines [], out []]
              (if (>= start n)
                (conj out (if (empty? lines) empty-paragraph {:length (- n p) :lines lines}))
                (let [end (suggest-break ts start width)
                      nl? (= 10 (cf-string-char-at cfs (dec end)))
                      e   (if nl? (dec end) end)
                      lines (conj lines {:start (- start p) :end (- e p)})]
                  (if nl?
                    (recur end end [] (conj out {:length (- e p) :lines lines}))
                    (recur end p lines out)))))
            (finally (cf-release ts)))))
      (finally (cf-release cfs)))))

(defn wrap
  "Break each of the paragraphs `texts` (strings without newlines) in
  `font` into lines no wider than `width` pixels. For each, returns
  {:length n :lines [{:start u :end u}]}, n its length in UTF-16 units; an
  empty paragraph has one empty line."
  [font texts width]
  (let [long? #(> (count %) (quot *window* 2))
        flush (fn [out batch] (if (seq batch) (into out (wrap-together font batch width)) out))]
    (loop [[t & more :as ts] (seq texts), batch [], size 0, out []]
      (cond
        (nil? ts)   (flush out batch)
        (long? t)   (recur more [] 0 (conj (flush out batch) (wrap-one font t width)))
        (> (+ size (count t)) *window*) (recur ts [] 0 (flush out batch))
        :else       (recur more (conj batch t) (+ size (count t) 1) out)))))

(defn- line-starting-at
  "The index of the line in `lines` (sorted) that starts at `u`, or nil."
  [lines u]
  (loop [lo 0, hi (dec (count lines))]
    (when (<= lo hi)
      (let [mid (quot (+ lo hi) 2)
            s (:start (nth lines mid))]
        (cond (= s u) mid
              (< s u) (recur (inc mid) hi)
              :else   (recur lo (dec mid)))))))

(defn rewrap
  "Line breaks for `text`, an edit of a paragraph `old` ({:length :lines},
  as `wrap` gives them) which replaced old's UTF-16 range [a, ob) with what
  is now [a, nb). Re-breaks only from the line before the edit until a
  break falls where one did before, shifted by the edit, and keeps old's
  lines past there. So an edit to a long paragraph costs about a window,
  not the paragraph."
  [font old text width a ob nb]
  (let [olines (:lines old)]
    (if (or (zero? (:length old)) (empty? text))
      (wrap-one font text width)
      (let [cfs (cf-string text)
            n   (cf-string-length cfs)
            delta (- nb ob)
            ;; the line holding the edit, and one before: an edit at a
            ;; line's start can let its first word fit on the line above
            j0 (loop [j (dec (count olines))]
                 (if (and (pos? j) (> (:start (nth olines j)) a)) (recur (dec j)) j))
            r  (max 0 (dec j0))
            ;; where old's breaks resume: a line ending at or past the edit
            ;; where an old line started has the old text after it
            resync (fn [{:keys [end]}] (when (>= end nb) (line-starting-at olines (- end delta))))
            ;; breaks usually resume within a few lines, so start small
            [fresh stopped] (try (lines-from font cfs (:start (nth olines r)) n width
                                             (quot *window* 4) resync)
                                 (finally (cf-release cfs)))
            kept (when stopped
                   (mapv (fn [ln] (-> ln (update :start + delta) (update :end + delta)))
                         (subvec olines (resync stopped))))]
        {:length n :lines (-> (subvec olines 0 r) (into fresh) (into kept))}))))

(defn utf16-length
  "How many UTF-16 units string `s` takes."
  [s]
  (reduce (fn [n c] (if (>= (int c) 0x10000) (+ n 2) (inc n))) 0 s))

(defn composed-range
  "The [start end) UTF-16 range of the user-perceived character (grapheme
  cluster) covering index `i` of string `text`."
  [text i]
  (let [cfs (cf-string text)]
    (try
      (ffi/with-layout [r cfrange]
        (cf-string-composed-range r cfs i)
        (let [loc (ffi/read-field r cfrange :location)]
          [loc (+ loc (ffi/read-field r cfrange :length))]))
      (finally (cf-release cfs)))))

;; ---------------------------------------------------------------- lines

(defn make-line
  "`text` (no newlines) set as one CTLine, or nil for \"\". Release it with
  `release`."
  [font text]
  (when (seq text)
    (let [cfs  (cf-string text)
          astr (try (cf-attributed-string-create ffi/null cfs (:attrs font))
                    (finally (cf-release cfs)))]
      (try (ct-line-create astr) (finally (cf-release astr))))))

;; The functions below take a line as {:line ctline :base u}: the CTLine
;; counts from 0 where its paragraph counts from :base.

(defn offset-for-index
  "Horizontal pixel offset of the caret before the paragraph's UTF-16 index
  `i` on line `ln`."
  [{:keys [line base]} i]
  (if line (ct-line-offset-for-index line (- i base) ffi/null) 0.0))

(defn index-for-position
  "The paragraph's UTF-16 index nearest pixel offset `x` on line `ln`, or nil."
  [{:keys [line base]} x]
  (when line
    (ffi/with-layout [p cgpoint]
      (ffi/write-field p cgpoint :x (double x))
      (ffi/write-field p cgpoint :y 0.0)
      (let [i (ct-line-index-for-position line p)]
        (when-not (neg? i) (+ i base))))))

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
