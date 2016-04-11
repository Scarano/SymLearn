(ns genlearn.util.repl-history
  (:import [java.util Properties]
           [java.io FileInputStream]))

(defn -main []
  (let [p (doto (Properties.)
            (.load (FileInputStream. ".settings/ccw.repl.cmdhistory.prefs")))
        commands (read-string (.getProperty p "cmdhistory"))]
    (doseq [c commands #_(reverse commands)]
      (println c)
      (println))))

(-main)














