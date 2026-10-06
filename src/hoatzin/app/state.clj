(ns hoatzin.app.state
  "The app map every hoatzin.app namespace works on, its defaults, and the
  small helpers they all share.")

(def defaults
  ;; The fonts are in :settings (see hoatzin.app.settings): the editor's, for
  ;; the text, and the UI's, for everything else.
  {:status-padding 4           ; points above and below the status bar's text
   :margin      24             ; points
   :blink-ms    530            ; the macOS caret blink period
   :blink-idle-ms 30000        ; the caret stops blinking after this long without input
   :wheel-lines 3
   :scrollbar-width 14         ; points: the bar's column at the right edge
   :thumb-width 6              ; points; wider while the bar is in use
   :thumb-width-active 10
   :thumb-min   32             ; points: the shortest the thumb gets
   :autoscroll-ms 50           ; how often a drag held outside the text scrolls
   :live-wrap-chars 400000     ; past this many characters, the text re-wraps to a
                               ; new width only once the width has held for
   :wrap-delay-ms 120          ; this long, rather than as the window is dragged:
                               ; each means wrapping the whole text again
   :font-delay-ms 150          ; how long a changed editor font setting waits
                               ; for the next change before it applies: each
                               ; means wrapping the whole text again
   :list-rows   8              ; the most options a dropdown's list shows at once
   :list-padding 4             ; points above and below each option's text
   :list-glide-ms 45           ; how quickly a dropdown's list scrolls to where it
                               ; is headed: it covers about two thirds of the
                               ; way in this long
   :scroll-glide-ms 90         ; how quickly the wheel scrolls the text to where it
                               ; is headed
   :frame-ms    8              ; how often a gliding list moves on
   :type-ahead-ms 1000         ; how long typing into a dropdown's list waits
                               ; for the next letter of the same option's name
   ;; the colours are the theme's, as the settings name it (see
   ;; hoatzin.app.theme); these are what the app has until it first syncs
   :background  [30 30 46]
   :foreground  [235 230 220]
   :selection   [76 84 128]
   :selection-unfocused [58 60 80]
   :scrollbar-track [38 38 56]
   :scrollbar-thumb [78 80 102]
   :scrollbar-thumb-active [128 132 158]
   :status-background [24 24 37]
   :status-foreground [170 168 190]
   :window-background [24 24 37] ; the windows over the text
   :ui-dim      [170 168 190]  ; read-only fields' text, a dropdown's arrow
   :ui-border   [96 98 128]    ; boxes' colours, where their style sets none
   :ui-accent   [128 132 158]
   :ui-focus    [150 158 230]  ; the border of the field with the focus
   :ui-field-background [24 24 37]
   :ui-hover    [40 40 60]     ; a dropdown's background under the pointer
   :ui-highlight [76 84 128]}) ; the option a dropdown's list is on

;; The app is a map:
;;   :renderer :density-fn :clipboard-fn   supplied by the host
;;   :mode                                 :normal, :insert or :command
;;   :command                              the command line's text, after `:`
;;   :message                              shown in the status bar until the
;;                                         next keystroke
;;   :buffers :next-buffer-id              every buffer, and the number the
;;                                         next one made is to have: see
;;                                         hoatzin.app.buffers
;;   :buffer-id :buffer-name :scratch?     the current buffer's number, its
;;                                         name while it has no file, and
;;                                         whether it is the scratch buffer
;;   :dir                                  its directory, if known
;;   :dialog-dir                           the directory the last file
;;                                         dialog chose in, for any buffer
;;   :path                                 the file loaded or saved, if any
;;   :saved                                the text as it is in that file
;;   :modified? :compared                  whether the text differs from
;;                                         :saved, as of :compared [text saved]
;;   :modes                                the modes, by name: see
;;                                         hoatzin.app.modes
;;   :major-mode                           the name of the current buffer's
;;                                         mode, or nil
;;   :minor-modes                          the names of the minor modes it
;;                                         is in, a set
;;   :variants :dims :next-variant-id      its variants, by id, its dim text,
;;   :variant-edit                         the id the next of either is to
;;                                         have, and the variant being typed:
;;                                         see hoatzin.app.variants
;;   :tag :tag-color                       a short name the buffers window
;;                                         shows the buffer with, and its colour
;;   :caret-shape                          :underline for the caret to be one,
;;                                         else nil
;;   :insets :inset :next-inset-id         the current buffer's insets, the
;;   :before? :xsel                        one the caret is in (before?: it
;;                                         is before it), what is selected
;;                                         across them (hoatzin.app.xsel),
;;   :saved-insets :inset-ctx              the number
;;                                         the next is to have, what they
;;                                         are in its file, and their text's
;;                                         layout context: see
;;                                         hoatzin.app.insets
;;   :confirm                              the question the status bar asks,
;;                                         or nil: see hoatzin.app.confirm
;;   :pointer                              where the pointer last moved, in
;;                                         the window
;;   :settings                             what the user can change: see
;;                                         hoatzin.app.settings
;;   :density :font                        the editor font, at the current
;;                                         density
;;   :fonts                                the :settings fonts :font and :ui
;;                                         were made from
;;   :applied-theme                       the name of the theme whose colours
;;                                         (see hoatzin.app.theme) are in
;;                                         the keys of `defaults`
;;   :fonts-at                             when (ms) the editor font setting,
;;                                         just changed, is to be applied
;;   :ui :ui-textures                      the UI font's {:font :metrics
;;                                         :lines}, :lines an atom caching
;;                                         texts set as lines in it, and its
;;                                         texture cache
;;   :ctx :layout :laid-out                layout context, layout, its text
;;   :textures :scratch                    line texture cache, FFI scratch
;;   :undo :undo-tail :undo-chain :mode-count
;;                                         the edit history: see
;;                                         hoatzin.app.history
;;   :doc :goal-x                          the document (see
;;                                         hoatzin.lib.editor); column for
;;                                         up/down
;;   :upstream?                            the caret, at a wrap point, is drawn
;;                                         at the end of the line above
;;   :selecting?                           the mark is active, as Emacs has it:
;;                                         the keys that move the caret
;;                                         extend the selection, as with shift
;;   :dragging? :drag-word :drag-point     a click is extending the selection
;;                                         (by words, from :drag-word [lo hi]),
;;                                         the pointer last at :drag-point [x y]
;;   :hover? :grab                         the pointer is over the scroll bar;
;;                                         the thumb is held :grab px below its top
;;   :composition                          {:text :cursor} while composing, or nil
;;   :window                               the window open over the text
;;                                         (:settings, :help or :buffers), or
;;                                         nil
;;   :help-scroll                          the help window's scroll, in rows
;;   :buffers-active :buffers-scroll       the buffers window's buffer, and
;;                                         its scroll in rows
;;   :blocks                               boxes in the text (see
;;                                         hoatzin.lib.ui), by id: each sits
;;                                         below the paragraph holding the
;;                                         document's mark of that id
;;   :floats                               boxes above everything, placed in
;;                                         the window, last on top
;;   :ui-values                            interactive boxes' values, by :id
;;   :focus                                the :id of the field or dropdown
;;                                         with the focus, or nil
;;   :list                                 the list open below the dropdown
;;                                         with the focus, or nil: see
;;                                         hoatzin.app.dropdown
;;   :font-families                        the fonts installed, from
;;                                         :font-families-fn, once the
;;                                         settings window has wanted them
;;   :option-faces                         an atom caching faces (see
;;                                         hoatzin.app.face) to show font
;;                                         families' names in, by [family size]
;;   :block-places :float-places           where they are, as
;;                                         hoatzin.app.boxes places them; the
;;                                         floats placed include the command
;;                                         line's hints
;;   :ui-hover? :ui-hover-id               the pointer is over a box; the
;;                                         :id of the interactive one it is
;;                                         over, if any
;;   :size :scroll                         output size and scroll, in pixels
;;   :wrap-width :wrap-at                  the width the text is to wrap to,
;;                                         and when, while a long text waits
;;                                         for its new width to hold
;;   :scroll-target :scroll-pos :scroll-at the scroll the wheel is gliding to, the
;;                                         exact scroll on the way and when it
;;                                         last moved
;;   :focused? :blink-from                 the caret blinks from :blink-from
;;   :dirty? :drawn-phase                  redraw needed / caret phase drawn
;;   :follow?                              scroll the caret into view
;;   :quit?
;; plus the keys of `defaults`.

(defn px
  "`pts` points in render pixels."
  [app pts]
  (long (Math/round (* (double pts) (:density app)))))

(defn insert? [app] (= :insert (:mode app)))
(defn command? [app] (= :command (:mode app)))

(defn touched
  "After an edit or caret move: show the caret solid and keep it in view."
  [app now]
  (assoc app :follow? true :dirty? true :blink-from now))

(defn enter-mode
  "The app in `mode`: out of normal mode, the mark is no longer active."
  [app now mode]
  (-> app
      (assoc :mode mode :dirty? true :blink-from now)
      (cond-> (not= :normal mode) (dissoc :selecting?))
      (update :mode-count (fnil inc 0))))
