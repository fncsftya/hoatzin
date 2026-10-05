(ns hoatzin.lib.macos
  "What SDL leaves to macOS's menu bar, through the Objective-C runtime."
  (:require [jolt.ffi :as ffi]))

(ffi/defcfn ^:private get-class "objc_getClass" [:string] :iptr)
(ffi/defcfn ^:private selector "sel_registerName" [:string] :iptr)
(ffi/defcfn ^:private send0 "objc_msgSend" [:iptr :iptr] :iptr)
(ffi/defcfn ^:private send-index "objc_msgSend" [:iptr :iptr :long] :iptr)
(ffi/defcfn ^:private send1 "objc_msgSend" [:iptr :iptr :iptr] :iptr)
(ffi/defcfn ^:private send-count "objc_msgSend" [:iptr :iptr] :long)
(ffi/defcfn ^:private cf-string "CFStringCreateWithCString" [:pointer :string :uint] :iptr)

(defn- ns-string [s] (cf-string ffi/null s 0x08000100))

(defn free-keys!
  "Take the key equivalent off the menu item titled `title` (SDL gives the
  app a menu with Minimize on cmd+m, which the editor wants for itself).
  Nothing happens if there is no such item."
  [title]
  (let [app  (send0 (get-class "NSApplication") (selector "sharedApplication"))
        menu (when-not (zero? app) (send0 app (selector "mainMenu")))]
    (when (and menu (not (zero? menu)))
      (let [t (ns-string title)]
        (dotimes [i (send-count menu (selector "numberOfItems"))]
          (let [sub (send0 (send-index menu (selector "itemAtIndex:") i) (selector "submenu"))]
            (when-not (zero? sub)
              (let [item (send1 sub (selector "itemWithTitle:") t)]
                (when-not (zero? item)
                  (send1 item (selector "setKeyEquivalent:") (ns-string "")))))))))))
