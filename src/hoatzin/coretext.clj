(ns hoatzin.coretext
  "Text rasterization through CoreText (macOS only).

  Lines are drawn into a CoreGraphics bitmap as white, premultiplied RGBA;
  tint them on the GPU (e.g. SDL_SetTextureColorMod)."
  (:require [jolt.ffi :as ffi]))

;; ---------------------------------------------------------------- bindings

;; CF object refs travel as :iptr, not :pointer: CoreFoundation hands back
;; tagged pointers (top bit set) for small strings and numbers, which an
;; unsigned :pointer reads as a BigInt that can't be passed back to C.

(def ^:private UTF8 0x08000100)                    ; kCFStringEncodingUTF8
(def ^:private ALPHA-PREMULTIPLIED-LAST 1)         ; kCGImageAlphaPremultipliedLast
(def ^:private BYTE-ORDER-32-BIG (bit-shift-left 4 12)) ; kCGBitmapByteOrder32Big

(ffi/defcfn ^:private cf-release "CFRelease" [:iptr] :void)
(ffi/defcfn ^:private cf-string-create "CFStringCreateWithCString"
  [:pointer :string :uint] :iptr)
(ffi/defcfn ^:private cf-dictionary-create "CFDictionaryCreate"
  [:pointer :pointer :pointer :long :pointer :pointer] :iptr)
(ffi/defcfn ^:private cf-attributed-string-create "CFAttributedStringCreate"
  [:pointer :iptr :iptr] :iptr)

(ffi/defcfn ^:private ct-font-create-with-name "CTFontCreateWithName"
  [:iptr :double :pointer] :iptr)
(ffi/defcfn ^:private ct-font-copy-family-name "CTFontCopyFamilyName" [:iptr] :iptr)
(ffi/defcfn ^:private ct-line-create "CTLineCreateWithAttributedString" [:iptr] :iptr)
(ffi/defcfn ^:private ct-line-typographic-bounds "CTLineGetTypographicBounds"
  [:iptr :pointer :pointer :pointer] :double)
(ffi/defcfn ^:private ct-line-draw "CTLineDraw" [:iptr :pointer] :void)

(ffi/defcfn ^:private cg-color-space-create-srgb "CGColorSpaceCreateWithName" [:iptr] :pointer)
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

(ffi/defcfn ^:private cf-string-get-cstring "CFStringGetCString"
  [:iptr :pointer :long :uint] :bool)

(defn- global
  "The value of a global constant (a CFTypeRef) exported by a framework."
  [sym]
  (ffi/read (or (ffi/find-symbol sym)
                (throw (ex-info (str "symbol not found: " sym) {:symbol sym})))
            :iptr))

(defn- symbol-address [sym]
  (or (ffi/find-symbol sym)
      (throw (ex-info (str "symbol not found: " sym) {:symbol sym}))))

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

;; ---------------------------------------------------------------- fonts

(def default-family
  "A serif face that ships with every macOS install."
  "Georgia")

(defn font
  "Open a CTFont by PostScript or family name at `size` (in pixels).
  Release it with `release-font`."
  ([size] (font default-family size))
  ([family size]
   (let [name (cf-string family)]
     (try
       (let [f (ct-font-create-with-name name (double size) ffi/null)]
         (when (zero? f) (throw (ex-info "CTFontCreateWithName failed" {:family family})))
         f)
       (finally (cf-release name))))))

(defn family-name
  "The family CoreText actually resolved — it substitutes for unknown names."
  [font]
  (let [ref (ct-font-copy-family-name font)]
    (try (cf-string->str ref) (finally (cf-release ref)))))

(defn release-font [font] (cf-release font))

;; ---------------------------------------------------------------- lines

(defn- ct-line
  "A CTLine for `text` in `font`, coloured from the drawing context."
  [font text]
  (let [{:keys [font-attr fg-from-context cf-true dict-key-callbacks dict-val-callbacks]} @constants
        s (cf-string text)]
    (try
      (ffi/with-alloc [ks (* 2 (ffi/sizeof :pointer))]
        (ffi/with-alloc [vs (* 2 (ffi/sizeof :pointer))]
          (ffi/write ks :iptr font-attr 0)
          (ffi/write ks :iptr fg-from-context 8)
          (ffi/write vs :iptr font 0)
          (ffi/write vs :iptr cf-true 8)
          (let [attrs (cf-dictionary-create ffi/null ks vs 2
                                            dict-key-callbacks dict-val-callbacks)]
            (try
              (let [astr (cf-attributed-string-create ffi/null s attrs)]
                (try (ct-line-create astr) (finally (cf-release astr))))
              (finally (cf-release attrs))))))
      (finally (cf-release s)))))

(defn rasterize
  "Render one line of `text` in `font` into a new RGBA bitmap.

  Returns {:pixels ptr :width w :height h :pitch bytes :ascent a :descent d};
  free :pixels with ffi/free once it has been uploaded."
  [font text]
  (let [line (ct-line font text)]
    (try
      (ffi/with-out [pa :double]
        (ffi/with-out [pd :double]
          (ffi/with-out [pl :double]
            (let [advance (ct-line-typographic-bounds line pa pd pl)
                  ascent  (ffi/read pa :double)
                  descent (ffi/read pd :double)
                  pad     2                          ; room for glyph overhang
                  w       (+ (long (Math/ceil advance)) (* 2 pad))
                  h       (+ (long (Math/ceil (+ ascent descent))) (* 2 pad))
                  pitch   (* 4 w)
                  pixels  (ffi/alloc (* pitch h))
                  cs      (cg-color-space-create-srgb (:srgb @constants))
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
                (cg-context-set-fill ctx 1.0 1.0 1.0 1.0)
                ;; CG's origin is bottom-left; the baseline sits `descent` up.
                (cg-context-set-text-position ctx (double pad) (+ pad descent))
                (ct-line-draw line ctx)
                (finally (cg-context-release ctx)))
              {:pixels pixels :width w :height h :pitch pitch
               :ascent ascent :descent descent}))))
      (finally (cf-release line)))))
