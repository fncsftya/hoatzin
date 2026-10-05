(ns hoatzin.app.state
  "The app map every hoatzin.app namespace works on, its defaults, and the
  small helpers they all share.")

(def defaults
  ;; The fonts are in :settings (see hoatzin.app.settings): the editor's, for
  ;; the text, and the UI's, for everything else.
  {:status-padding 4           ; points above and below the status bar's text
   :margin      24             ; points
   :blink-ms    530            ; the macOS caret blink period
   :wheel-lines 3
   :scrollbar-width 14         ; points: the bar's column at the right edge
   :thumb-width 6              ; points; wider while the bar is in use
   :thumb-width-active 10
   :thumb-min   32             ; points: the shortest the thumb gets
   :autoscroll-ms 50           ; how often a drag held outside the text scrolls
   :font-delay-ms 150          ; how long a changed editor font setting waits
                               ; for the next change before it applies: each
                               ; means wrapping the whole text again
   :list-rows   8              ; the most options a dropdown's list shows at once
   :list-padding 4             ; points above and below each option's text
   :list-glide-ms 45           ; how quickly a dropdown's list scrolls to where it
                               ; is headed: it covers about two thirds of the
                               ; way in this long
   :frame-ms    8              ; how often a gliding list moves on
   :type-ahead-ms 1000         ; how long typing into a dropdown's list waits
                               ; for the next letter of the same option's name
   :background  [30 30 46]
   :foreground  [235 230 220]
   :selection   [76 84 128]
   :selection-unfocused [58 60 80]
   :scrollbar-track [38 38 56]
   :scrollbar-thumb [78 80 102]
   :scrollbar-thumb-active [128 132 158]
   :status-background [24 24 37]
   :status-foreground [170 168 190]
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
;;   :path                                 the file loaded or saved, if any
;;   :saved                                the text as it is in that file
;;   :modified? :compared                  whether the text differs from
;;                                         :saved, as of :compared [text saved]
;;   :settings                             what the user can change: see
;;                                         hoatzin.app.settings
;;   :density :font                        the editor font, at the current
;;                                         density
;;   :fonts                                the :settings fonts :font and :ui
;;                                         were made from
;;   :fonts-at                             when (ms) the editor font setting,
;;                                         just changed, is to be applied
;;   :ui :ui-textures                      the UI font's {:font :metrics
;;                                         :lines}, :lines an atom caching
;;                                         texts set as lines in it, and its
;;                                         texture cache
;;   :ctx :layout :laid-out                layout context, layout, its text
;;   :textures :scratch                    line texture cache, FFI scratch
;;   :doc :goal-x                          the document (see
;;                                         hoatzin.lib.editor); column for
;;                                         up/down
;;   :upstream?                            the caret, at a wrap point, is drawn
;;                                         at the end of the line above
;;   :dragging? :drag-word :drag-point     a click is extending the selection
;;                                         (by words, from :drag-word [lo hi]),
;;                                         the pointer last at :drag-point [x y]
;;   :hover? :grab                         the pointer is over the scroll bar;
;;                                         the thumb is held :grab px below its top
;;   :composition                          {:text :cursor} while composing, or nil
;;   :window                               the window open over the text
;;                                         (:settings), or nil
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

(defn enter-mode [app now mode]
  (assoc app :mode mode :dirty? true :blink-from now))
