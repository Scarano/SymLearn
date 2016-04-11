(ns genlearn.util.config
  (:require [clojure [test :refer :all]
                     [string :as string]]
            [genlearn [util :refer [to-string
                                    map-leaves]]])
  (:import [clojure.lang Keyword
                         IFn]
           [genlearn.util IConfig
                          RunConfig
                          RunConfigGroup]))

(set! *warn-on-reflection* true)


; A wrapper around RunConfig, which reads param/value mappings, and allows you to do grid
; search over all allowed values of all parameters.
;
; Differences from RunConfig:
;  - Parameter values are pre-computed and accessed using keywords, for faster access
;    from inner loops.
;  - One getValue method (instead of different methods for different param types).
;  - Parameter values may have clojure code that refers to other parameter values.
;
; A completely Clojure-based solution in which the configuration file is itself a valid
; Clojure expression would be much nicer, but I already had RunConfig written in Java.

(deftype Config [runConfig paramMap]
  IConfig
  (getValue [this ^Keyword param-key]
    (let [value (paramMap param-key)]
      (if (nil? value)
        (throw (Error. (str "Invalid configuration key " param-key)))
        value)))

  (getValue [this ^String param-prefix ^Keyword param-suffix]
    (.getValue this param-prefix param-suffix))

  IFn
  (invoke [this param-key]
    (.getValue this ^Keyword param-key))
  (invoke [this param-prefix param-suffix]
    (.getValue this ^String (name param-prefix)
                    ^String (name param-suffix))))

(defn- get-config-value [^RunConfig config param-name]
  (let [str-val (.getString config param-name)]
    (cond
      (#{"T" "F"} str-val)
        (= str-val "T")
      (re-matches #"[0-9\-].*" str-val)
        (read-string str-val)
      (re-matches #"\(.*\)" str-val)
        (eval (map-leaves #(if (and (symbol? %) (= (first (name %)) \$))
                             (get-config-value config (subs (name %) 1))
                             %)
                (read-string str-val)))
      :else
        str-val)))

(defn make-config-from-run-config [^RunConfig config]
  (let [param-map (into {} (for [param-name (.getParameterNames config)]
                             [(keyword param-name) (get-config-value config param-name)]))]
    (Config. config param-map)))

(defn make-configs [config-file]
  (for [run-config (RunConfig/readRunConfigs config-file)]
    (make-config-from-run-config run-config)))

(defn -main [config-file]
  (doseq [config (make-configs config-file)]
    (doseq [param (.paramMap ^Config config)]
      (println (str (name (first param)) ": " (second param))))
    (println)))

(defn dump-config [^IConfig config]
  (doseq [[k v] (:paramMap config)]
    (printf "%s: %v\n" (name k) v)))


(deftype MockConfig [paramKeys paramMap]
  IConfig
  (getValue [this ^Keyword param-key]
    (let [value (paramMap param-key)]
      (if (nil? value)
        (throw (Error. (str "Invalid configuration key " param-key)))
        value)))

  (getValue [this ^String param-prefix ^Keyword param-suffix]
    (.getValue this param-prefix param-suffix))

  IFn
  (invoke [this param-key]
    (.getValue this ^Keyword param-key))
  (invoke [this param-prefix param-suffix]
    (.getValue this ^String (name param-prefix)
                    ^String (name param-suffix))))

(defn make-mock-config [param-mappings]
  (MockConfig. (map first param-mappings) (into {} param-mappings)))
















