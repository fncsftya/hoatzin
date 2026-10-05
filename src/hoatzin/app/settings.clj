(ns hoatzin.app.settings
  "The settings the user can change, and the file they persist in.

  Settings are a map:
    {:editor-font {:family s :size n}   the text's font
     :ui-font     {:family s :size n}   everything else's: the status bar,
                                        the command line and boxes
     :theme       s}                    the name of a theme's, see
                                        hoatzin.app.theme
  sizes in points. Each setting is at a path into the map, and `schema`
  says what each may be and what it is by default.

  They persist as JSON keyed as the map is, in settings.json in the
  hoatzin directory of the XDG config home (~/.config unless
  $XDG_CONFIG_HOME says otherwise). A setting the file leaves out, or that
  isn't valid there, takes its default, as do all of them when there is no
  file."
  (:require [babashka.fs :as fs]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hoatzin.app.theme :as theme]
            [hoatzin.lib.coretext :as ct]))

(def font-sizes "The font sizes allowed, in points: [min max]." [6 72])

(defn- font-size? [v]
  (let [[lo hi] font-sizes] (and (integer? v) (<= lo v hi))))

(defn- family? [v] (and (string? v) (not (str/blank? v))))

(def schema
  "Every setting: its :path, its :default and :valid?, whether a value may
  be it."
  [{:path [:editor-font :family] :default ct/default-family :valid? family?}
   {:path [:editor-font :size]   :default 20                 :valid? font-size?}
   {:path [:ui-font :family]     :default "Menlo"            :valid? family?}
   {:path [:ui-font :size]       :default 13                 :valid? font-size?}
   {:path [:theme]               :default theme/default-name :valid? (set theme/names)}])

(def defaults
  (reduce (fn [s {:keys [path default]}] (assoc-in s path default)) {} schema))

(def ^:private by-path (into {} (map (juxt :path identity)) schema))

(defn valid?
  "Whether `v` may be the setting at `path`."
  [path v]
  (boolean (some-> (by-path path) :valid? (as-> ok? (ok? v)))))

(defn change
  "`settings` with the setting at `path` set to `v`, or nil if `v` may not
  be it."
  [settings path v]
  (when (valid? path v) (assoc-in settings path v)))

(defn from-data
  "Settings from `data`, as read from JSON with keyword keys: each setting
  as `data` has it, where that is valid, else its default."
  [data]
  (reduce (fn [s {:keys [path valid?]}]
            (let [v (when (map? data) (get-in data path))]
              (if (valid? v) (assoc-in s path v) s)))
          defaults schema))

(defn read-json
  "Settings from JSON text `s`."
  [s]
  (from-data (json/read-str s :key-fn keyword)))

(defn write-json
  "Settings as JSON text, indented for reading."
  [settings]
  (str (json/write-str settings :indent true) "\n"))

;; ---------------------------------------------------------------- the file

(defn file
  "Where settings persist."
  []
  (str (fs/path (fs/xdg-config-home "hoatzin") "settings.json")))

(defn read-file
  "{:settings s} from the file at `path`: defaults if there is none. If it
  can't be read, the defaults and :error, why not."
  [path]
  (if-not (fs/exists? path)
    {:settings defaults}
    (try {:settings (read-json (slurp path))}
         (catch Exception e {:settings defaults :error (ex-message e)}))))

(defn write-file!
  "Write `settings` to the file at `path`, making its directory if need
  be: nil, or why it could not."
  [path settings]
  (try (some-> (fs/parent path) fs/create-dirs)
       (spit path (write-json settings))
       nil
       (catch Exception e (ex-message e))))
