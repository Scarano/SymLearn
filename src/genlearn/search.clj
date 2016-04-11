(ns genlearn.search
  (:require [genlearn.util :refer :all]
            [genlearn.util.config :refer [make-configs
                                          make-mock-config]]
            [genlearn.types :refer [make-type
                                    function-type
                                    var-type
                                    unify-independent-types
                                    function-type?
                                    identity-type?
                                    find-arity
                                    normalize-vars
                                    substitute
                                    returned-type]]
            [genlearn.typedrouterlogic :refer [computation-limit
                                               routing-combinator?
                                               enumerate-routers
                                               make-router-mem
                                               router-type
                                               null-router?
                                               router-arity
                                               make-app
                                               ->Value
                                               reduce-expr
                                               expand-primitives
                                               infer-type
                                               expr-string
                                               make-primitive
                                               primitive-library
                                               primitive-index
                                               preprocess
                                               typed-eval
                                               fast-eval
                                               limited-eval
                                               example-fact]]
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
                     AppEntry
                     ComputationLimitExceededException)
           (genlearn.util Pair)
           (java.lang NullPointerException
                      ArithmeticException)
           #_(java.util HashMap
                       ArrayList)))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)
(set! *unchecked-math* true)

(def- debug false)


(defn find-valid-function-types [prims]
  "Lists the types of all primitives and partially applied primitives.
  For example, + adds both (:int -> :int -> :int) and (:int -> :int) to the list."
  (distinct-fast 
    (for [t (map :type prims)
          t' ((fn partials [^IType t'] (when (function-type? t')
                                         (cons t' (lazy-seq (partials (.rhs t'))))))
              t)]
      (normalize-vars t'))))

(defn index-by-type [values]
  (reduce (fn [index ^IValue value]
            (update index (.type value) #(cons value %)))
          {}
          values))


; At this point, the following constants are used only by extract-exprs-to-depth, which is 
; used only for debugging purposes.
(def ^:const M1 (exp2 -20)) ; can be thought of as arity penalty
(def ^:const logM1 (log M1))
(def ^:const branch-penalty (double 1))
(def ^:const depth-penalty 1.0)

(declare type-node-instance)

(defrecord SearchState [primitives applicable-types
                        node-index node-queue expr-cache]
  ISearchState
  (^ITypeNode typeNodeInstance [this ^IType type]
    (type-node-instance this type))
  (^java.util.HashMap exprCache [this]
    expr-cache)

  Object
  (toString [this]
    (to-string primitives "\n"
               applicable-types "\n"
               node-index "\n"
               @node-queue)))

(def latest-search-state (atom nil))

(defn initialize-search-state [primitives]
  (let [state (SearchState. primitives (find-valid-function-types primitives)
                            (java.util.HashMap.) (ref '()) (java.util.HashMap.))]
    (reset! latest-search-state state)
    state))

(defrecord PrimEntry [prim cond-prob])

; The exprs member is a cache of all expressions (of a particular type) with a score better
; than min-score.
(defrecord ExprCache [min-score exprs])

(defrecord TypeNode [^IType type primitives app-entries expr-cache]
  ITypeNode
  Object
  (toString [this]
;    (to-string "{Node<" type "> " primitives "}")))
;    (to-string "{Node<" type "> " primitives " " (while-realized app-entries) "}")))
    (to-string "{Node<" type "> " primitives "; " app-entries "}")))

(defn print-app-entries [^TypeNode type-node]
  (println (to-string (.type type-node) ":"))
  (doseq [app (:app-entries type-node)]
    (println (to-string "  " app))))

(defn primitives-of-type [state type]
  "List primitives that can be used wherever type is used (i.e., unify without substitutions)"
  (for [prim (:primitives state)
        :let [bindings (unify-independent-types type (:type prim))]
        :when (and (some? bindings)
                   (= type (->> type (substitute bindings) normalize-vars)))]
    prim))

(defn primitives-of-exact-type [state type]
  "List primitives whose type is the specified type. This is an alternative to 
  primitives-of-type, which currently performs worse on the polynomial task because it
  generates so many redundant expressions."
  (for [prim (:primitives state)
        :when (= (:type prim) type)]
    prim))

(declare enumerate-apps)

(defn make-type-node [state type]
  (let [prims (primitives-of-type state type)]
  (TypeNode. type 
;             (for [prim prims] (PrimEntry. prim ((:id prim) (:primitive-lps state))))
             (sort-by (comp - :score) prims)
             (enumerate-apps state type)
             (atom (ExprCache. java.lang.Double/MAX_VALUE nil)))))

(defn type-node-instance [state type]
  (indexed-instance-unsync (:node-index state) type #(make-type-node state type)))

(defn matching-rhs-types [state ^IType query-type]
  (cons query-type
        (when (function-type? query-type)
          (for [app-type (:applicable-types state)
                :let [bindings (unify-independent-types query-type app-type)]
                :when bindings]
            app-type))))

(defn routers-up-to-arity [^long n]
  (mapcat #(enumerate-routers %) (range 0 (inc n))))

(reset! debug-print-active false)
;(declare enumerate-apps')
;(defn enumerate-apps [& args] (last (repeatedly 3 #(apply enumerate-apps' args))))
(defn enumerate-apps [state ^IType type]
#_  (println (to-string type))
  (debug-print enumerate-apps-debug (to-string "> enumerate-apps (" type ")"))
  (let [req-router-type (normalize-vars (function-type (var-type -2)
                                                       (function-type (var-type -1)
                                                                      type)))]
    (doall ; XXX
    (distinct-fast
      (for [router-str (routers-up-to-arity (find-arity type))
            :let [router (make-router-mem router-str)
                  r-bindings (unify-independent-types req-router-type (:type router))
                  _ (when (nil? r-bindings)
                      (println (to-string "! enumerate-apps: Can't unify req-router-type "
                                          req-router-type " with " (:type router))))
                  router-type (normalize-vars (substitute r-bindings req-router-type))]
            :let [_ (debug-print enumerate-apps-debug
                                 (to-string "  enumerate-apps for-loop: <" req-router-type "> "
                                            router " <" router-type ">"))]
            :when (some? r-bindings)
            lhs-type-unsub (:applicable-types state)
            :let [app-bindings (unify-independent-types (.lhs router-type) lhs-type-unsub)]
            :when (some? app-bindings)
            :let [app-type (->> router-type
                             (substitute app-bindings)
                             (normalize-vars))
                  lhs-type (.lhs app-type)
                  _ (debug-print enumerate-apps-debug 
                                 (to-string "  enumerate-apps for-loop-2: <" lhs-type "> "
                                            app-bindings))]
            rhs-type (matching-rhs-types state (.lhs (.rhs app-type)))
            
            ; We know (router lhs-type rhs-type) unifies with type; now make sure it's 
            ; identical to type. (fixes "BS + (+ 1)" bug; also stops returning duplicate exprs.)
            :let [reconstructed-type (-> router-type
                                       (returned-type lhs-type)
                                       (returned-type rhs-type))]
            :when (= type reconstructed-type)
            
            ; Don't allow I (identity) values that are not routed down by routers
            ; This reduces redundancy among expressions a bit.
            :when (cond
                    (not (re-find #"[SB]" router-str))
                      (not (identity-type? rhs-type))
                    (not (re-find #"[SC]" router-str))
                      (not (identity-type? lhs-type))
                    :else
                      true)]
        (do 
          (debug-print enumerate-apps-debug 
            (to-string "  enumerate-apps adding app "
                       "(" router " <" lhs-type "> <" rhs-type ">)"))
          (AppEntry. state router (normalize-vars lhs-type) (normalize-vars rhs-type))))))))

(defn merge-prims-and-apps
  "unused"
  ([prims apps ^double app-trigger-score]
    (debug-print "> merge-prims-and-apps-3" (count prims) (first prims) app-trigger-score)
    (if (or (empty? prims)
            (< (:score (first prims)) app-trigger-score))
      (debug-pre-print "  allowing apps"
        (merge-prims-and-apps prims apps))
      (debug-pre-print "  next prim (" (first prims) ")"
        (lazy-seq (cons (first prims)
                        (merge-prims-and-apps (rest prims) apps app-trigger-score))))))
  ([prims apps]
    (debug-print "> merge-prims-and-apps-2" (count prims) (first prims))
    (cond
      (and (empty? prims) (empty? apps))
        (debug-pre-print "  end both"
           nil)
      (empty? prims)
        (debug-pre-print "  end prims"
           apps)
      (empty? apps)
        (debug-pre-print "  end prims"
           prims)
      (< (:score (first prims)) (:score (first apps)))
        (debug-pre-print "  app wins (" (first apps) ")"
          (lazy-seq (cons (first apps) (merge-prims-and-apps prims (rest apps)))))
      :else
        (debug-pre-print "  prim wins (" (first prims) ")"
          (lazy-seq (cons (first prims) (merge-prims-and-apps (rest prims) apps)))))))

(reset! debug-print-active true)

(defn extract-exprs-to-depth [state ^TypeNode type-node ^long max-depth]
  (debug-print "> extract-exprs-to-depth" (.type type-node))
  (doall
    (concat
      (do
        (doseq [e (:primitives type-node)]
          (debug-print " -extract-exprs-to-depth" (.type type-node) ":" (expr-string e)))
        (:primitives type-node))
      (if (= max-depth 1)
        '()
        (for [app-entry-obj (:app-entries type-node)
              :let [app-entry ^AppEntry app-entry-obj
                    router (.router app-entry)
                    lhs-node (.lhs app-entry)
                    rhs-node (.rhs app-entry)
                    router-cost (* logM1 (+ branch-penalty (router-arity router)))]
              lhs (extract-exprs-to-depth state lhs-node (dec max-depth))
              rhs (extract-exprs-to-depth state rhs-node (dec max-depth))]
          (let [app (make-app router lhs rhs router-cost)]
            (let [t (infer-type app)
                  b (when (some? t) (unify-independent-types t (.type type-node)))]
              (when (nil? b)
                (debug-print "! extract-exprs " 
                             (.type type-node) "(" router
                                                   (to-string (:type lhs-node)) ","
                                                   (to-string (:type rhs-node)) ")"
                             "made app" router lhs rhs
                             "of incorrect type (" (to-string t) ")")))
            (debug-print " =extract-exprs-to-depth" (.type type-node) ":" (expr-string app))
            app))))))

(defn extract-exprs-of-type-to-depth [state type ^long max-depth]
  (extract-exprs-to-depth state (type-node-instance state type) max-depth))

(reset! debug-print-active false)

(declare extract-exprs-to-score)

(def typecheck-exprs false)
(defn extract-exprs-to-score' [config state ^TypeNode type-node ^double min-score]
  (debug-print "> extract-exprs" (.type type-node) min-score)
  (if-not (neg? min-score)
    '()
    (let [logM1 (config :search.logM1)
          bp (config :search.branch-penalty)
          dp (config :search.depth-penalty)
          restrict-I (config :search.restrict-identity-primitive)]
      (distinct-fast ; XXX some day I should figure out how to make this unnecessary
        (doall ; doesn't really matter but may make profiling easier
          (concat
            (filter #(> (:score %) min-score) (:primitives type-node))
            (for [app-entry-obj (:app-entries type-node)
                  :let [app-entry ^AppEntry app-entry-obj
                        router (.router app-entry)
                        allow-I-as-lhs (or (not restrict-I) (re-find #"[SC]" (name (.id router))))
                        allow-I-as-rhs (or (not restrict-I) (re-find #"[SB]" (name (.id router))))
                        lhs-node (.lhs app-entry)
                        rhs-node (.rhs app-entry)
                        router-cost (* logM1 (+ bp (router-arity router)))
                        min-cost (+ router-cost logM1 logM1 dp)]
                  :when (> min-cost min-score)
                
                  :let [lhs-exprs (cond->> (extract-exprs-to-score config state lhs-node
                                             (- min-score router-cost logM1 dp))
                                    (not allow-I-as-lhs)
                                      (filter #(or (not (instance? IValue %))
                                                   (not= :I (.id ^IValue %)))))
                        #_ (debug-print "lhs:" (count lhs-exprs))]
                  lhs lhs-exprs
                
                  :let [rhs-exprs (cond->> (extract-exprs-to-score config state rhs-node
                                             (- min-score router-cost (.score ^IExpr lhs) dp))
                                    (not allow-I-as-rhs)
                                      (filter #(or (not (instance? IValue %))
                                                   (not= :I (.id ^IValue %)))))
                        #_ (debug-print "  rhs:" (count rhs-exprs))]
                  rhs rhs-exprs]
              (let [_ (debug-print router " " (expr-string lhs) " " (expr-string rhs) "; "
                                   router-cost " " (:score lhs) " " (:score rhs))
                    app (make-app router lhs rhs router-cost)]
                (when typecheck-exprs
                  (let [t (infer-type app)
                        b (when (some? t) (unify-independent-types t (.type type-node)))]
                    (when (nil? b)
                      (debug-print "! extract-exprs " 
                                   (.type type-node) "(" router
                                                         (to-string (:type lhs-node)) ","
                                                         (to-string (:type rhs-node)) ")"
                                   "made app" router lhs rhs
                                   "of incorrect type (" (to-string t) ")"))))
                app))))))))

(defn extract-exprs-to-score [config ^SearchState state ^TypeNode type-node ^double min-score]
  "Extract all expressions with a score above min-score"
  (let [cache @(:expr-cache type-node)]
    (cond
      (= (:min-score cache) min-score)
        (:exprs cache)
      (< (:min-score cache) min-score)
        (doall (take-while #(>= (:score %) min-score) (:exprs cache)))
      :else
        (let [exprs (extract-exprs-to-score' config state type-node min-score)]
          (swap! (:expr-cache type-node) 
                 (fn [cache]
                   (if (< min-score (:min-score cache))
                     (ExprCache. min-score (sort-by #(- (.score ^IExpr %)) exprs))
                     cache)))
          exprs))))
  
; old memoized version that spent too much time calculating type hashes & type equality
#_(defn extract-exprs-to-score [^SearchState state ^TypeNode type-node ^double min-score]
   "Extract all expressions with a score above min-score"
   (let [cache (.exprCache state)
         expr-entry ^Pair (.get cache (.type type-node))]
     (if (and (some? expr-entry) (<= (.first expr-entry) min-score))
       (doall (take-while #(>= (:score %) min-score) (.second expr-entry)))
       (let [exprs (extract-exprs-to-score' state type-node min-score)]
         (.put cache (.type type-node) (Pair. min-score (sort-by #(- (.score ^IExpr %)) exprs)))
         exprs))))

#_(defn extract-exprs-to-score [^SearchState state ^TypeNode type-node ^double min-score]
  "Extract all expressions with a score above min-score"
  (let [cache (.exprCache state)
        call-key (Pair. (.type type-node) min-score)
        exprs (.get cache call-key)]
    (if (some? exprs)
      exprs
      (let [exprs (extract-exprs-to-score' state type-node min-score)]
        #_(println "cache miss" (count cache))
        (.put cache call-key exprs)
        exprs))))

(reset! debug-print-active false)

;(declare extract-exprs-of-type-to-score')
;(defn extract-exprs-of-type-to-score [& args]
;  (last (repeatedly 3 #(apply extract-exprs-of-type-to-score' args))))
(defn extract-exprs-of-type-to-score [config state type ^double min-score]
  "Extract all expressions of specified type with a score above min-score"
  (doall (extract-exprs-to-score config state (type-node-instance state type) min-score)))

(defn extract-exprs
  "no good, don't use"
  ([state ^TypeNode type-node]
    (debug-pre-print "> extract-exprs" (.type type-node)
      (merge-prims-and-apps
        (:primitives type-node)
        (cons
          {:score -100}
          (lazy-seq
            (sort-by (comp - :score)
              (for [[router lhs-type rhs-type] (:app-entries type-node)
                    :let [lhs-node (@(:node-index state) lhs-type)
                          rhs-node (@(:node-index state) rhs-type)]
                    :when (and (some? lhs-node) (some? rhs-node))
                    lhs (extract-exprs state lhs-node)
                    rhs (extract-exprs state rhs-node)]
                (debug-pre-print "  extract-exprs" (.type type-node) " making app" router lhs rhs
                  (make-app router lhs rhs))))))
        (double -100)))))
;        (* 2 logM1)))))

(defn extract-exprs-of-type [state ^IType type]
  (extract-exprs state (type-node-instance state type)))

(def debug-primitives (map (fn [[id score]] (assoc (primitive-index id) :score (log score)))
                           [[:I     0.02]
                            [:1     0.02]
                            [:range 0.02]
                            [:count 0.02]]))

(def minimal-primitives (map (fn [[id score]] (assoc (primitive-index id) :score (log score)))
                             [[:I     0.1]
                              [:1     0.1]
                              [:+     0.1]]))

(def all-primitives (map #(assoc % :score (log (/ 0.25 (count primitive-library))))
                         primitive-library))

(defn debug1 []
  (let [state (initialize-search-state debug-primitives)
        node-a-a (type-node-instance state (make-type '(a a)))
        node-int-int (type-node-instance state (make-type '(:int :int)))
        node-int (type-node-instance state (make-type :int))
        exprs (extract-exprs state node-int)]
   (println)
   (println (to-string state))
;   (println (to-string "exprs: " (take 10 exprs)))
   (doseq [[i expr] (map vector (range) (take 10 exprs))]
     (println (to-string "expr " i ": " expr)))))

(defn debug2 []
  (let [state (initialize-search-state debug-primitives)
        node-int (type-node-instance state (make-type :int))
        node-int-int (type-node-instance state (make-type '(:int :int)))
        exprs1 (extract-exprs-to-depth state node-int 1)
        exprs2 (extract-exprs-to-depth state node-int 2)]
    (doseq [expr (sort-by (comp - :score)
                          (extract-exprs-to-depth state node-int 3))]
      (println (format "%.3f" (:score expr))
               (to-string ">>> " (expr-string expr) ": " (typed-eval expr))))
    state))

(defn debug-min-score [& args]
  (let [min-score (Integer/parseInt (first args))
        state (initialize-search-state debug-primitives)
        node-int (type-node-instance state (make-type :int))
        node-int-int (type-node-instance state (make-type '(:int :int)))]
    (doseq [expr (extract-exprs-to-score state node-int min-score)]
      (println (format "%.3f" (:score expr))
               (to-string ">>> " (expr-string expr) ": " (typed-eval expr))))
    state))

(defn extract-exprs-benchmark [^IType type min-score]
  (dotimes [_ 3]
    (let [state (initialize-search-state debug-primitives)]
      (time (count (doall (extract-exprs-of-type-to-score state type min-score)))))))

#_(extract-exprs-benchmark (make-type :int) -10)

#_(defn -main [& args]
   (time (apply debug-min-score args)))


#_(defrecord Task [desc samples])

(reset! debug-print-active true)

(def computation-limit-exceeded-counter (atom 0))

(defn ^IValue apply-pessimistically [^IValue func arg]
  (try
    (.apply func arg false nil)
    (catch ComputationLimitExceededException e
      (swap! computation-limit-exceeded-counter inc)
      (println (format "Computation limit exceeded (%d total times)"
                       @computation-limit-exceeded-counter))
      nil)
    (catch Exception e
      (if (or (instance? NullPointerException e)
              (instance? ArithmeticException e))
        (debug-print "Error applying expression:" (expr-string (.source func))
                     "to argument:" (to-string arg))
        (do
          (print-stack-trace e)
          (println "Expression:" (expr-string (.source func)))
          (println "Full expression:" (to-string (.source func)))
          (println "Arg:" (to-string arg))
#_        (println "Type application entries:")
#_        (let [state @latest-search-state
                node-index @(:node-index state)]
            (doseq [k (keys node-index)]
              (println (to-string k ":"))
              (doseq [app (:app-entries (node-index k))]
                (println (to-string "  " app)))))))
      nil)))

(reset! debug-print-active false)

(defn solvers-by-problem [problems funcs domain]
  (let [solvers (java.util.HashMap.)]
    (doseq [problem problems]
      (let [results (for [x domain] (problem (.denotation ^IValue x)))]
        (.put solvers results (java.util.ArrayList.))))
    (doseq [func funcs]
      (let [results (for [x domain
                          :let [result (apply-pessimistically func x)]
                          :while (some? result)]
                      (.denotation result))
            matches (.get solvers results)]
        (when matches
          (.add ^java.util.ArrayList matches func))))
    (into {} (for [[results funcs] solvers] [results (vec funcs)]))))

(deftest solvers-by-problem-test
  (let [solvers (solvers-by-problem
                  [identity inc]
                  (map #(->Value (str %) nil (make-type '(:int :int)) % 0)
                       [identity - (comp - -)])
                  (map preprocess (range 4)))
        solvers-ident (solvers '(0 1 2 3))
        solvers-inc (solvers '(1 2 3 4))]
    (is (and (= (count solvers-ident) 2)
             (= (count solvers-inc) 0)
             (= ((:denotation (first solvers-ident)) 3) 3)))))

(defrecord Problem [desc f]
  clojure.lang.IFn
  (invoke [this x] (f x))
  (invoke [this x y] (f x y))
  (applyTo [this args] (apply f args))
  Object
  (toString [this] (to-string desc)))

(defn polynomial-problems [N max-order max-coeff seed]
  (let [random (java.util.Random. seed)]
    (doall
      (repeatedly N
        (fn []
          (let [order (.nextInt random (inc max-order))
                coeffs (repeatedly (inc order) #(.nextInt random (inc max-coeff)))]
            (Problem. coeffs
                   (fn [x]
                     (apply + (for [[exp coeff] (map vector (range) coeffs)]
                                (* coeff (apply * (repeat exp x)))))))))))))

(defn polynomial-task-generator [N max-order max-coeff seed]
  "Returns infinite lazy seq of length-N lists of polynomial problems"
  (for [seed' (range seed)]
    (polynomial-problems N max-order max-coeff seed')))

(reset! debug-print-active true)

(defn generate-at-least-n-exprs [config primitives type N-min]
  (let [state (initialize-search-state primitives)]
    (loop [min-score -1.0
           last-count 0]
      (let [exprs (distinct-fast (extract-exprs-of-type-to-score config state type min-score))
            expr-count (count exprs)]
        (when (> expr-count last-count)
          (debug-print (format "generate-at-least-n-exprs min-score %.2f: %d exprs"
                               min-score (count exprs))))
        (if (>= expr-count N-min)
          exprs
          (recur (- min-score 1.0) expr-count))))))

(reset! debug-print-active false)

(defn generate-exprs [config primitives type N]
  (let [exprs (to-array (generate-at-least-n-exprs config primitives type N))
        sorted-exprs (sort-by (comp - :score) exprs)]
    (take N sorted-exprs)))

;; Used to tune M1 parameter
(defn count-type-nodes [config primitives type min-score]
  (time
    (let [state (initialize-search-state primitives)
          exprs (extract-exprs-of-type-to-score config state type min-score)]
      (println "M1 = 2 ^ " (config :search.log2-M1))
      (println (count exprs) "expressions")
      (println (count @(:node-index state)) "type nodes"))))

(defn assign-primitive-probs [primitives prob-mass max-noise seed]
  (let [random (java.util.Random. seed)
        n (count primitives)
        weights (repeatedly n #(/ (+ 1.0 (* (- (.nextDouble random) 0.5) max-noise)) n))
        probs (map #(* prob-mass %) (normalize weights))]
    (doall (map (fn [prim prob] (assoc prim :score (log prob))) primitives probs))))

(def poly-task-primitives 
  (let [prim-values (map #(primitive-index %) [:I :0 :1 :+ :*])]
    (assign-primitive-probs prim-values 0.25 0.0 1)))

(deftest poly-task-primitives-smoketest
  (is (= (count poly-task-primitives) 5)))

(reset! debug-print-active false)

(defn count-subexpressions
  ([exprs opaque-primitives] (count-subexpressions exprs opaque-primitives 1.0))
  ([exprs opaque-primitives weight]
    (debug-print "> count-subexpressions" (map expr-string exprs))
    (reduce (fn [counter e]
              (debug-print " count-subexpressions" (expr-string e) ":\n"
                           (string/join "\n" (sort (map (fn [[e c]]
                                                          (str "    " (expr-string e) ": " c))
                                                       counter))))
              (reduce-expr (fn [counter' e']
                             ; if e' is a Value and a primitive, use source expr; else use e'
                             ; (for non-primitives, (.source e) = e)
                             (let [e' (if (instance? IValue e') (.source ^IValue e') e')]
                               (debug-print "  incrementing" (expr-string e')
                                            ":" (counter' e'))
                               (assoc counter' e' (+ weight (or (counter' e') 0.0)))))
                           counter
                           e))
            {}
            (if opaque-primitives exprs (map expand-primitives exprs)))))

(reset! debug-print-active true)

(deftest count-subexpressions-test
  (let [counter (count-subexpressions (list (preprocess '((C + 1) (neg 1)))
                                            (preprocess '(+ (neg 1)))) false)]
    (is (= (count counter) 7))
    (is (= (counter (preprocess 1)) 3.0))
    (is (= (counter (preprocess '(neg 1))) 2.0))))

(defn expr-size
  ([expr]
    (expr-size expr 0))
  ([expr acc]
    (if (instance? IValue expr)
      (inc acc)
      (let [app ^IApp expr
            acc' (expr-size (.lhs app) acc)]
        (recur (.rhs app) acc')))))

(defprotocol SolverSelector
  (add [this solvers])
  (get-selected-solvers [this]))

(deftype ShortestSolverSelector [selected-solvers]
  SolverSelector
  (add [this solvers]
    (if (empty? solvers)
      this
      (ShortestSolverSelector. 
        (conj selected-solvers
           (argmin (fn [^IValue x] (expr-size (.source x))) solvers)))))
  (get-selected-solvers [this]
    selected-solvers))

(defn make-shortest-solver-selector [] (ShortestSolverSelector. []))

(deftype FewestNewSubexpressionsSelector [selected-solvers ^java.util.HashSet subexprs]
  SolverSelector
  (add [this solvers]
    (if (empty? solvers)
      this
      (let [selected (argmin (fn [^IValue solver]
                               (reduce-expr (fn [c ^IValue expr]
                                              (if (.contains subexprs expr) c (inc c)))
                                            0
                                            (.source solver)))
                             solvers)]
        (reduce-expr (fn [_ expr] (.add subexprs expr)) nil selected)
        (FewestNewSubexpressionsSelector. (conj! selected-solvers selected) subexprs))))
  (get-selected-solvers [this]
    (persistent! selected-solvers)))

(defn make-fewest-new-subexpressions-selector []
  (FewestNewSubexpressionsSelector. (transient []) (java.util.HashSet.)))

(deftype FewestUniqueSubexpressionsSelector [solver-lists ^java.util.HashSet subexprs]
  SolverSelector
  (add [this solvers]
    (if (empty? solvers)
      this
      (do
        (doseq [solver solvers]
          (reduce-expr (fn [_ expr] (.add subexprs expr)) nil solver))
        (FewestNewSubexpressionsSelector. (conj! solver-lists solvers) subexprs))))
  (get-selected-solvers [this]
    (map (fn [solvers]
           (argmin (fn [^IValue solver]
                     (reduce-expr (fn [c ^IValue expr] ;; XXX not finished yet
                                    )
                                  0
                                  (.source solver)))))
         (persistent! solver-lists))))

(defn make-fewest-unique-subexpressions-selector []
  (FewestNewSubexpressionsSelector. (transient []) (java.util.HashSet.)))

(defn shortest-solver-strategy [selected-solvers task-candidates]
  "Used in the reduce in next-generation-primitives
  OLD - need to be adapted to implement SolverSelector"
  (if (empty? task-candidates)
    selected-solvers
    (conj selected-solvers
           (argmin (fn [^IValue x] (expr-size (.source x))) task-candidates))))

(reset! debug-print-active false)

(defn shortest-n-solvers-strategy [n selected-solvers task-candidates]
  "Used in the reduce in next-generation-primitives
  OLD - need to be adapted to implement SolverSelector"
  (if (empty? task-candidates)
    selected-solvers
    (let [selected-for-task (top-n (fn [^IValue x] (- (expr-size (.source x)))) n task-candidates)]
      (debug-print "solvers:" (map (fn [^IValue x] (expr-string (.source x))) selected-for-task))
      (concat selected-solvers selected-for-task))))

(defn short-expr-string [expr]
  (if (instance? IApp expr)
    (str "[" (string/replace (expr-string expr) " " "") "]")
    (expr-string expr)))

(def opaque-primitives true)

(defn perform-search-iteration
  [config primitives type task domain frontier-size max-primitives solver-selector
   primitive-generation-smoothing]
  (let [candidate-exprs (generate-exprs config primitives type frontier-size)
        candidates (map limited-eval candidate-exprs)
        #_ (doseq [f candidates]
           (println (expr-string (.source f)))
           (println "  " (.apply ^IValue f (preprocess 0) false nil)))
        solver-lists (solvers-by-problem task candidates domain)
        #_ (doseq [[results solvers] solver-lists]
           (when (not (empty? solvers))
             (println results)
             (doseq [solver solvers]
               (println (to-string (.source solver)) ">>" (to-string solver)))))
        solvers (get-selected-solvers
                  (reduce (fn [sel solvers] (add sel solvers))
                          solver-selector (vals solver-lists)))
        counter (count-subexpressions (map (fn [^IValue x] (.source x)) solvers) opaque-primitives)
        #_ (doseq [solver solvers]
            (debug-print (map #(.denotation (.apply ^IValue solver (preprocess %) false nil))
                              (range 10))
                         "\n  "
                         (expr-string (.source solver))))
        #_ (doseq [[expr c] (sort-by val counter)]
           (println c (expr-string expr)))
        multi-use-exprs (filter (fn [[_ c]] (> c 1.0)) counter)
        [top-exprs top-expr-counts] (transpose (top-n val max-primitives multi-use-exprs))
        probs (normalize top-expr-counts 0.25)
        new-prim-dist (smooth-log (zipmap top-exprs (map log probs))
                                  (map (fn [^IValue prim]
                                         (vector (.source prim) (.score prim))) 
                                       primitives)
                                  primitive-generation-smoothing)
        new-prims (map (fn [[expr prob]] (->Value (keyword (short-expr-string expr))
                                                  expr
                                                  (infer-type expr)
                                                  (:denotation (limited-eval expr))
                                                  prob))
                       (sort-by (comp - second) 
                                (filter #(> (second %) Double/NEGATIVE_INFINITY)
                                        new-prim-dist)))]
    {:unique-problems (count solver-lists)
     :solved-problems (count (filter #(not (empty? (val %))) solver-lists))
     :new-primitives new-prims
     :debug-solver-lists solver-lists}))

(defn debug-first-poly-iteration
  [& {:keys [primitives
             type
             task
             domain
             frontier-size
             max-primitives
             solver-selector
             log2-M1
             gen-smoothing]
      :or {primitives poly-task-primitives
           type (make-type '(:int :int))
           task (polynomial-problems 10000 2 9 123)
           domain (map preprocess (range 10))
           frontier-size 10000
           max-primitives 30
           solver-selector (make-fewest-new-subexpressions-selector)
           log2-M1 -20
           gen-smoothing 1.0}}]
  (perform-search-iteration
    (make-mock-config {:search.log2-M1 log2-M1
                       :search.M1 (exp2 log2-M1)
                       :search.logM1 (log (exp2 log2-M1))
                       :search.branch-penalty 1.0
                       :search.depth-penalty (* 1.0 (log (exp2 log2-M1)))})
    primitives type task domain frontier-size max-primitives solver-selector gen-smoothing))

(defn- next-generation-primitives
  "old, keeping this wrapper version in case I need to use some dead code that still refers to it"
  [primitives type task domain frontier-size solver-selector]
  (:primitives (perform-search-iteration primitives type task domain frontier-size
                                     30 solver-selector)))

(defn sample-function [^IValue f]
  (let [t (.type f)]
    (cond
      (= t (make-type :int))
        (.denotation f)
      (= t (make-type '(:int :int)))
        (map (.denotation f) (range 10))
      (= t (make-type '(:int :int :int)))
        (for [x (range 4)]
          (for [y (range 4)]
            (((.denotation f) x) y))))))

(defn run-search [config task-generator initial-primitives type
              domain frontier-size max-primitives iterations primitive-generation-smoothing]
  (loop [t 1
         tasks task-generator
         primitives initial-primitives]
    (println "Iteration" t)
    (let [task (first tasks)
          {:keys [new-primitives unique-problems solved-problems]}
                    (time (perform-search-iteration config primitives type task domain frontier-size
                                                max-primitives
                                                (make-fewest-new-subexpressions-selector)
                                                primitive-generation-smoothing))]
      (println solved-problems "of" unique-problems "solved.")
        
      (println "Primitives:")
      (doseq [prim (sort-by #(vector (- (:score %)) (:id %)) new-primitives)]
        (println " " (format "<%.2f>" (:score prim)) (name (:id prim)))
        (println "   " (sample-function prim)))
      
      (println)
      
      (when (< t iterations)
        (recur (inc t) (rest tasks) new-primitives)))))
  
(deftest run-search-smoketest
  (let [output (with-out-str (run-search (make-mock-config {:search.M1 (exp2 -20)
                                                        :search.logM1 (log (exp2 -20))
                                                        :search.branch-penalty 1.0
                                                        :search.depth-penalty (log (exp2 -20))})
                                     (repeat (polynomial-problems 10000 2 9 123))
                                     poly-task-primitives
                                     (make-type '(:int :int))
                                     (map preprocess (range 10))
                                     100 30 2 1.0))]
    (is (nil? (re-find #"Exception" output)))
    (is (= 2 (count (re-seq #"Iteration" output))))))

#_(def solvers (next-generation-primitives poly-task-primitives
                                         (make-type '(:int :int))
                                         (polynomial-tasks 20 2 5 123)
                                         (map preprocess (range 10))
                                         100
                                         shortest-solver-strategy))
;(time
;  (doseq [solver solvers]
;    (println (expr-string (:source solver)))))

#_(defn -main [min-score-str]
   (let [min-score (Integer/parseInt min-score-str)
         state (initialize-search-state poly-task-primitives)
         exprs (extract-exprs-of-type-to-score state (make-type '(:int :int)) min-score)]
     (println (count exprs))
       (doseq [expr (take-last 1000 exprs)]
         (println (format "%.2f" (:score expr)) ">>" (expr-string expr))
         (try
           (print-string " " (.apply ^IValue (typed-eval expr ) (preprocess 0) false nil))
           (catch Exception e
             (println expr)
             (print-string (:type expr))
             (doseq [k (keys @(:node-index state))]
               (println (to-string k ":"))
               (doseq [app (:app-entries (@(:node-index state) k))]
                 (println (to-string "  " app)))))))))

; count returned exprs vs distinct exprs
#_(defn -main []
   (let [state (initialize-search-state poly-task-primitives)
         exprs (extract-exprs-of-type-to-depth state (make-type '(:int :int)) 3)]
     (println (count exprs))
     (let [expr-strings (sort (map expr-string exprs))]
       (println (count (distinct expr-strings)))
       (doseq [expr-string (take-last 400 expr-strings)]
         (println expr-string)))))

#_(defn reproduce-bs++1-bug []
   (assert (is (= 3 (count minimal-primitives))))
   (let [state (initialize-search-state minimal-primitives)
         exprs (generate-exprs minimal-primitives (make-type '(:int :int)) 300)]
;        exprs (extract-exprs-of-type-to-depth state (make-type '(:int :int)) 4)]
    (doseq [expr exprs]
      (println (expr-string expr) ":" (to-string (infer-type expr)))
      (apply-pessimistically (fast-eval expr) (preprocess 0)))))

#_(defn -main []
   (reproduce-bs++1-bug))

(defn -main [frontier-size-str iterations]
 (time
   (run-search #_(repeat (polynomial-problems 10000 2 9 123))
           (map #(polynomial-problems 10000 2 9 %) (iterate #(+ 57 %) 123))
;           poly-task-primitives
            all-primitives
;            (assign-primitive-probs primitive-library 0.25 0.4 1)
            (make-type '(:int :int))
            (map preprocess (range 10))
            (Integer/parseInt frontier-size-str)
            30
            (Integer/parseInt iterations)
            1.0)))

(defn -main [config-file]
  (time
    (doseq [config (make-configs config-file)]
      (let [random-seeds (iterate #(+ 57 %) 123)
            task-type (case (config :task.name)
                        (make-type '(:int :int)))
            task-domain (map preprocess (config :task.domain))
            max-blocks (config :search.max-blocks)
            frontier-size (config :search.frontier-size)
            prims poly-task-primitives ;XXX should be config param
            block-smoothing (config :search.smoothing)
            num-problems (config :search.problem-count)
            task (case (config :task.name)
                   "poly" (map #(polynomial-problems num-problems
                                                     (config :task.poly.order)
                                                     (config :task.poly.max-coeff)
                                                     %)
                               random-seeds))
            iterations (config :search.iterations)]
        (run-search config task prims task-type task-domain 
                frontier-size max-blocks iterations block-smoothing)))))
                                

; performance debugging
#_(defn -main [min-score-str]
  (time
    (let [state  (initialize-search-state all-primitives)
;          exprs (extract-exprs-of-type-to-depth state (make-type '(:int :int)) 2)]
          exprs (extract-exprs-of-type-to-score state (make-type '(:int :int))
                                                (Integer/parseInt min-score-str))]
      (println "Exprs:" (count exprs)))))












