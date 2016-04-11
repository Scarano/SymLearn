(ns genlearn.maxenttest
  (:require [genlearn.util :refer [to-string]]
            [genlearn.types :refer [make-type]]
            [genlearn.typedrouterlogic :refer [->Value
                                               expr-string]]
            [plumbing.core :refer [distinct-fast]]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:import (genlearn IType
                     IExpr
                     IValue
                     IApp
                     ITypeNode
                     ISearchState
                     AppEntry)
           (java.util Collection
                      HashMap
                      ArrayList)
           (edu.stanford.nlp.classify LinearClassifier
                                      LinearClassifierFactory)
           (edu.stanford.nlp.ling BasicDatum
                                  Datum)))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)
(set! *unchecked-math* true)


(defrecord StoplightDatum [features label]
  Datum
  (asFeatures [this] features)
  (label [this] label)
  (labels [this] (list label)))

(defn make-stoplight [n-s e-w]
  (StoplightDatum. (list [:CONST 1] [:NS n-s] [:EW e-w]
                         ;[:CONST2 2] [:CONST3 3] [:CONST4 4] [:CONST5 5] [:CONST6 6] [:CONST7 7]
                         [:UNIQ (mod (System/nanoTime) 1000)]
                         [:CHEAT (= n-s e-w)]
                         #_[:CHEAT2 (= n-s e-w)]
                         )
                   (if (= n-s e-w) :broken :working)))

(defn stoplight-test []
  (let [training (list (make-stoplight :green :red)
                       (make-stoplight :green :red)
                       (make-stoplight :green :red)
                       (make-stoplight :red :green)
                       (make-stoplight :red :green)
                       (make-stoplight :red :green)
                       (make-stoplight :red :red))
        tests (list (make-stoplight :green :red)
                    (make-stoplight :red :green)
                    (make-stoplight :red :red)
                    (make-stoplight :green :green))
;        trainer (MaxEntModel/train training 10.0 true)
;        _ (doseq [datum training]
;        model (.train trainer training)
        
        factory (doto (LinearClassifierFactory.)
                  (.useConjugateGradientAscent)
                  (.setVerbose false)
                  (.setSigma 10.0))
        classifier (.trainClassifier factory ^Collection training)]
    #_(.dump classifier)
    (println)
    (doseq [datum tests]
      (printf "%s: %.2f; %.2f\n" (to-string (:features datum))
                                 (.scoreOf classifier ^Datum datum :working)
                                 (.scoreOf classifier ^Datum datum :broken))
      #_(printf "Class(%s): %s\n" (to-string (:features datum))
                                (.classOf classifier ^StoplightDatum datum))
      #_(.justificationOf classifier ^StoplightDatum datum))
    (println)
    (doseq [feature (.features classifier)]
      (printf "%s: %.2f; %.2f\n" (to-string feature) (.weight classifier feature :working)
                                                     (.weight classifier feature :broken)))
    (println)))
        

(defn -main []
  (time (stoplight-test)))














