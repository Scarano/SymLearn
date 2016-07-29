(ns genlearn.postsearch
  (:require [genlearn.util :refer [to-string
                                   fail
                                   debug-print-active
                                   when-debug-print-active
                                   debug-print
                                   debug-printf
                                   debug-pre-print
                                   debug-post-print
                                   aadd
                                   transpose
                                   argmin
                                   top-n
                                   entries-to-vectors
                                   log
                                   normalize
                                   normalize-log
                                   smooth-log
                                   indexed-instance-unsync]]
            [genlearn.util.config :refer [make-configs
                                          dump-config
                                          make-mock-config]]
            [genlearn.types :refer [make-type
                                    function-type
                                    var-type
                                    identity-type?
                                    function-type?
                                    var-type?
                                    find-arity
                                    returned-type
                                    normalize-vars
                                    substitute
                                    unify-independent-types]]
            [genlearn.search :refer [expr-size
                                     count-subexpressions
                                     apply-pessimistically
                                     short-expr-string]]
            [genlearn.typedrouterlogic :refer [->Value
                                               expr-string
                                               make-readable
                                               reduce-expr
                                               extract-values
                                               make-router-mem
                                               enumerate-routers
                                               router-arity
                                               infer-type
                                               primitive-index
                                               preprocess
                                               fast-eval
                                               typed-eval
                                               limited-eval]]
            [plumbing.core :refer [distinct-fast]]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:import (genlearn Information
                     IType
                     IExpr
                     IValue
                     IApp
                     ITypeNode
                     ISearchState
                     AppEntry
                     ScoredExpr)
           (genlearn.search Problem)
           (genlearn.typedrouterlogic App)
           (genlearn.util ObjectCounter)
           (edu.stanford.nlp.classify LinearClassifier
                                      LinearClassifierFactory)
           (edu.stanford.nlp.ling Datum)
           (java.util Collection
                      HashMap
                      HashSet
                      ArrayList)
           (java.lang Double)))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)
(set! *unchecked-math* true)



(defn find-valid-function-types [blocks]
  "Lists the types of all blocks and partially applied blocks.
  For example, + adds both (:int -> :int -> :int) and (:int -> :int) to the list."
  (distinct-fast 
    (for [t (map :type blocks)
          t' ((fn partials [^IType t'] (when (function-type? t')
                                         (cons t' (lazy-seq (partials (.rhs t'))))))
              t)]
      (normalize-vars t'))))

(declare type-node-instance)

(defrecord SearchState [blocks block-dist applicable-types
                        node-index node-queue expr-cache]
  ISearchState
  (^ITypeNode typeNodeInstance [this ^IType type]
    (type-node-instance this type))
  (^HashMap exprCache [this]
    expr-cache)

  Object
  (toString [this]
    (to-string block-dist "\n"
               applicable-types "\n"
               node-index "\n"
               @node-queue)))

(def latest-search-state (atom nil))

(defn se-expr [^ScoredExpr se]
  (.expr se))
(defn se-value [^ScoredExpr se]
  (.value se))
(defn se-score [^ScoredExpr se]
  (.score se))
(defn se-type [^ScoredExpr se]
  (.type (.expr se)))
(defn se-id [^ScoredExpr se]
  (.id (.value se)))
(defn se-source [^ScoredExpr se]
  (.source (.value se)))
(defn se-denotation [^ScoredExpr se]
  (.denotation (.value se)))

(defn blocks-of-dist [block-dist]
  (map se-value block-dist))

(defn value-string [^IValue value]
  (expr-string (.source value)))

(defn se-string [^ScoredExpr se]
  (format "%s <%.2f>"
    (let [expr (.expr se)]
      (if (instance? IValue expr)
        (value-string expr)
        (expr-string expr)))
    (.score se)))

(defn initialize-search-state [block-dist]
  (let [blocks (blocks-of-dist block-dist)
        state (SearchState. blocks block-dist
                            (find-valid-function-types blocks)
                            (HashMap.) (ref '()) (HashMap.))]
    (reset! latest-search-state state)
    state))

; The exprs member is a cache of all expressions (of a particular type) with a score better
; than min-score.
(defrecord ExprCache [min-score exprs])

(defrecord TypeNode [^IType type s-blocks app-entries expr-cache]
  ITypeNode
  Object
  (toString [this]
    (to-string "{Node<" type "> " s-blocks "; " app-entries "}")))

(defn print-app-entries [^TypeNode type-node]
  (println (to-string (.type type-node) ":"))
  (doseq [app (:app-entries type-node)]
    (println (to-string "  " app))))

(defn s-blocks-of-type [state type]
  "List primitives that can be used wherever type is used (i.e., unify without substitutions)"
  (for [s-block (:block-dist state)
        :let [bindings (unify-independent-types type (se-type s-block))]
        :when (and (some? bindings)
                   (= type (->> type (substitute bindings) normalize-vars)))]
    s-block))

(declare enumerate-apps)

(defn make-type-node [state type]
  (let [s-blocks (s-blocks-of-type state type)]
    (TypeNode. type
               (sort-by (comp - se-score) s-blocks)
               (enumerate-apps state type)
               (atom (ExprCache. Double/MAX_VALUE nil)))))

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
    (distinct-fast ; XXX distinct shouldn't be necessary...
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
          (AppEntry. state router (normalize-vars lhs-type) (normalize-vars rhs-type)))))))

(reset! debug-print-active true)

(defn extract-exprs-to-depth [state ^TypeNode type-node ^long max-depth]
  "This function is only used for debugging. (See extract-exprs-to-score.)"
  (debug-print "> extract-exprs-to-depth" (.type type-node))
  (doall
    (concat
      (do
        (doseq [e (:s-blocks type-node)]
          (debug-print " -extract-exprs-to-depth" (.type type-node) ":" 
                       (expr-string (se-expr e))))
        (:s-blocks type-node)) ;XXX
      (if (= max-depth 1)
        '()
        (for [app-entry-obj (:app-entries type-node)
              :let [app-entry ^AppEntry app-entry-obj
                    router (.router app-entry)
                    lhs-node (.lhs app-entry)
                    rhs-node (.rhs app-entry)
                    router-cost (* (log 0.25) (+ 1.0 (router-arity router)))]
              lhs (extract-exprs-to-depth state lhs-node (dec max-depth))
              rhs (extract-exprs-to-depth state rhs-node (dec max-depth))]
          (let [app (App. router lhs rhs Double/NaN)]
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
(def typecheck-exprs true)

(declare extract-exprs-to-score)

(defn extract-exprs-to-score' [state ^TypeNode type-node ^double min-score]
  "Generate all expressions for the requested type node with score (log prob) >= min-score.
  Actually, that's not true; as a heuristic to improve performance, a depth penalty
  based on logM1 is added to scores, even though that's not part of the real score of the
  expression. Therefore, the closer M1 is to 0, the more expressions with score >= min-score
  will be excluded due to excessive depth. It's weird, but it seems to help find a lot more
  expressions a lot faster this way."
  (debug-print "> extract-exprs" (.type type-node) min-score)
  (if-not (neg? min-score)
    '()
    (let [logM1 ^double (:logM1 state)
          bp ^double (:branch-penalty state)
          dp ^double (:depth-penalty state)]
      (distinct-fast ; XXX some day I should figure out how to make this unnecessary
                     ; (and then decide if the following doall is a good thing or a bad thing)
        (doall ; doesn't really matter but may make profiling easier
          (concat
            (filter #(> (se-score %) min-score) (:s-blocks type-node))
            (filter some?
              (for [app-entry-obj (:app-entries type-node)
                    :let [app-entry ^AppEntry app-entry-obj
                          router (.router app-entry)
                          lhs-node (.lhs app-entry)
                          rhs-node (.rhs app-entry)
                          router-cost (* logM1 (+ bp (router-arity router)))
                          min-cost (+ router-cost logM1 logM1 dp)]
                    :when (> min-cost min-score)
                    [lhs lhs-score] (extract-exprs-to-score state lhs-node
                                                            (- min-score router-cost logM1 dp))
                    [rhs rhs-score] (extract-exprs-to-score state rhs-node
                                                            (- min-score router-cost lhs-score dp))]
                (let [_ (debug-print router " " (expr-string lhs) " " (expr-string rhs) "; "
                                     router-cost " " lhs-score " " lhs-score)
                      app (App. router lhs rhs Double/NaN)
                      s-app (ScoredExpr. app (+ router-cost lhs-score rhs-score))]
                  (if typecheck-exprs
                    (let [t (try (infer-type app)
                              (catch Throwable e
                                (println "infer-type failed:")
                                (.printStackTrace e)
                                nil))
                          b (when (some? t) (unify-independent-types t (.type type-node)))]
                      (when (nil? b)
                        (println "! extract-exprs " 
                                     (to-string (.type type-node)) "(" router
                                                           (to-string (:type lhs-node)) ","
                                                           (to-string (:type rhs-node)) ")"
                                     "made app" router lhs rhs
                                     "of incorrect type (" (to-string t) ")")
                        (assert false))
                      #_ s-app
                      ;; If I decide to filter out the type <a>, I'll have to change the structure
                      ;; of the loop so that the for has :when that looks like the following:
                      (when-not (var-type? t) s-app))
                      ; (Or hack it by filtering out the nils returned by the above.)
                    s-app))))))))))

(defn extract-exprs-to-score [^SearchState state ^TypeNode type-node ^double min-score]
  "Extract all expressions with a score above min-score. Memoizing wrapper for
  extract-exprs-to-score'."
  (let [cache @(:expr-cache type-node)]
    (cond
      (= (:min-score cache) min-score)
      (:exprs cache)
      (< (:min-score cache) min-score)
      (doall (take-while #(>= (se-score %) min-score) (:exprs cache)))
      :else
        (let [exprs (extract-exprs-to-score' state type-node min-score)]
          (swap! (:expr-cache type-node) 
                 (fn [cache]
                   (if (< min-score (:min-score cache))
                     (ExprCache. min-score (sort-by #(- (se-score %)) exprs))
                     cache)))
          exprs))))
  
(reset! debug-print-active false)

;(declare extract-exprs-of-type-to-score')
;(defn extract-exprs-of-type-to-score [& args]
;  (last (repeatedly 3 #(apply extract-exprs-of-type-to-score' args))))
(defn extract-exprs-of-type-to-score [state type ^double min-score]
  "Extract all expressions of specified type with a score above min-score"
  (doall (extract-exprs-to-score state (type-node-instance state type) min-score)))

(reset! debug-print-active false)

(defn generate-at-least-n-exprs [config param-prefix block-dist type N-min]
  (let [state (-> (initialize-search-state block-dist)
                (assoc :logM1 (config param-prefix "logM1"))
                (assoc :branch-penalty (config param-prefix "branch-penalty"))
                (assoc :depth-penalty (config param-prefix "depth-penalty")))]
    (loop [min-score -1.0
           last-count 0]
      (let [exprs (distinct-fast (extract-exprs-of-type-to-score state type min-score))
            expr-count (count exprs)]
        (when (> expr-count last-count)
          (debug-print (format "generate-at-least-n-exprs min-score %.2f: %d exprs"
                               min-score (count exprs))))
        (if (>= expr-count N-min)
          exprs
          (recur (- min-score 1.0) expr-count))))))

(reset! debug-print-active false)

(defn generate-n-exprs [config param-prefix block-dist type N]
  (let [exprs (to-array (generate-at-least-n-exprs config param-prefix block-dist type N))
        sorted-exprs (sort-by (comp - se-score) exprs)]
    (take N sorted-exprs)))

(defn generate-exprs [config param-prefix block-dist type]
  (let [N (config param-prefix "frontier-size")]
    (generate-n-exprs config param-prefix block-dist type N)))


(defn shortest-selector [solver-sets]
  (for [solvers solver-sets]
    (if (empty? solvers)
      (list)
      (list
        (argmin (fn [^IValue x] (expr-size (.source x))) solvers)))))

(defn fewest-new-subexprs-selector [solver-sets]
  (let [subexprs (HashSet.)]
    (for [solvers solver-sets]
      (if (or (empty? solvers) (empty? (rest solvers)))
        solvers
        (list
          (let [solver* (argmin (fn [^IValue solver]
                                  (reduce-expr (fn [c ^IValue expr]
                                                 (if (.contains subexprs expr) c (inc c)))
                                               0
                                               (.source solver)))
                                solvers)]
            (reduce-expr (fn [_ expr] (.add subexprs expr)) nil solver*)
            solver*))))))

; XXX inefficient expr.equals()
(defn fewest-unique-subexprs-selector [solver-sets]
  (let [subexpr-counts (ObjectCounter.)]
    (doseq [solver (apply concat solver-sets)]
      (reduce-expr (fn [_ expr]
                     (.increment subexpr-counts ^Object expr))
                   nil
                   (.source ^IValue solver)))
    (for [solvers solver-sets]
      (if (or (empty? solvers) (empty? (rest solvers)))
        solvers
        (list
          (argmin (fn [^IValue solver]
                    (reduce-expr (fn [c expr]
                                   (if (> (.get subexpr-counts expr) 1) c (inc c)))
                                 0
                                 (.source solver)))
                  solvers))))))

(defn selector-by-key [k]
  (case k
    :shortest shortest-selector
    :fewest-new fewest-new-subexprs-selector
    :fewest-unique fewest-unique-subexprs-selector
    (fail "Invalid selector name: %s" (name k))))


(defn uniform-primitive-dist [selected-set library]
  (for [id selected-set
        :let [prim (or (library id)
                       (fail "uniform-primitive-dist: primitive not found: %s" id))]]
    (ScoredExpr. prim (log (/ 0.25 (count selected-set))))))

(defn generate-initial-hypotheses [config h-prims task-type]
  (let [exprs (filter some?
                      (map se-expr
                           (generate-exprs config "search.hypo" h-prims task-type)))
        hypotheses (doall (map limited-eval exprs))]
    ; Use same hypotheses for every problem
    (repeat hypotheses)))

(defn smooth-block-dists [bd1 bd2 lambda]
  (let [new-dist (smooth-log (map (fn [b] [(se-expr b) (se-score b)]) bd1)
                             (map (fn [b] [(se-expr b) (se-score b)]) bd2)
                             lambda)]
    (map (fn [[b log-p]] (ScoredExpr. b log-p)) new-dist)))

(defn generate-feature-candidates [config f-type f-prims f-blocks]
  (let [prior-smoothing (config :search.feat.prior-smoothing)
        blocks (smooth-block-dists f-blocks f-prims prior-smoothing)]
    ; XXX This doall may waste significant memory if many feature candidates are generated...?
    (doall (filter some?
                   (map (comp limited-eval se-expr)
                        (generate-exprs config "search.feat" blocks f-type))))))

(defn feature-values [feature problems]
  (for [problem problems
        :let [value (apply-pessimistically feature problem)]]
    (when (some? value) (.denotation value))))

(defn complement-feature [^IValue f]
  "Create feature g such that (g x) = (not (f x)).
  Also satisfies (= f (complement-feature (complement-feature f))), because if f is already
  wrapped in a negation, that negation will be removed, not wrapped in another negation."
  (typed-eval
    (let [source (.source f)]
      (if (and (instance? IApp source)
               (= (.id (.router ^IApp source)) :B)
               (instance? IValue (.lhs ^IApp source))
               (= (.id ^IValue (.lhs ^IApp source)) :not))
        (.rhs ^IApp source)
        (App. (make-router-mem "B") (primitive-index :not) source Double/NaN)))))

(deftest complement-feature-test
  (let [values (map (comp fast-eval preprocess) (range 4))
        features (map (comp fast-eval preprocess) '((C > 1) (C > 2)))
        features' (map complement-feature features)
        features'' (map complement-feature features')
        fv (for [f features] (feature-values f values))
        fv' (for [f features'] (feature-values f values))
        fv'' (for [f features''] (feature-values f values))]
    (is (= fv' (map #(map not %) fv))
        (= fv'' fv))))

(reset! debug-print-active true)

(defn select-features [config feature-candidates h-blocks problems solution-sets]
  (let [min-observations (config :search.feat.min-observations)
        selector (selector-by-key (keyword (config :search.feat.selector)))
        #_ (println ">select-features" (count feature-candidates))
        #_ (println (string/join "\n"
                     (for [f feature-candidates
                           :let [sf (.source ^IValue f)]]
                       (str "  " (expr-string sf) ": " (to-string (make-readable sf))))))
        h-block-indexes (into {} (map vector h-blocks (range))) ;XXX inefficient expr.equals()
        num-problems (count problems)
        ; XXX redundant - many features are the same as the last generation
        raw-feature-vectors (for [feature feature-candidates]
                              [feature (vec (feature-values feature problems))])
        _ (println "  select-features raw-feature-vectors" (count raw-feature-vectors))
        ; flip features that return mostly true, so they return mostly false
        feature-vectors (for [[f v] raw-feature-vectors
                              :let [trues (count (filter true? v))
                                    falses (count (filter false? v))
                                    not-v (map #(when (some? %) (not %)) v)]
                              :when (and (>= trues min-observations) (>= falses min-observations))]
                          (if (<= trues falses)
                            [f v]
                            [(complement-feature f) (vec not-v)]))
        _ (println "  select-features feature-vectors" (count feature-vectors))
        #_ (println (for [[f v] feature-vectors] (expr-string (.source ^IValue f))))
        #_ (println feature-vectors)
        feature-groups (group-by second feature-vectors) ;XXX inefficient expr.equals()
        _ (println "  select-features feature-groups" (count feature-groups))
        _ (doseq [[v group] feature-groups]
            (printf "%s:\n  %d; %s\n" v (count group) (value-string (first (first group))))) 
        selected-features (vec (selector
                                 (for [f-v-pairs (vals feature-groups)]
                                   (for [[f v] f-v-pairs] f))))
        selected-feature-vectors (keys feature-groups)
        ; In feature-matrix, rows are problems, columns are features
        feature-matrix (transpose vector selected-feature-vectors)
        #_ (println "  select-features feature-groups" (count feature-groups)
                                                      (first (keys feature-groups)))
        #_ (println "  select-features selected-feature-vectors"
                   (count selected-feature-vectors) (first selected-feature-vectors))
        #_ (println "  select-features feature-matrix" (count feature-matrix))
        observations
          (for [[solution-set feature-vector] (map vector solution-sets feature-matrix)
                :let [#_ (do (print "  select-features ")
                            (println (count solution-set) (count feature-vector)))
                      features (mapcat (fn [feature-idx value]
                                         (when value (list feature-idx)))
                                       (range) feature-vector)]
                solution solution-set
                block (extract-values (.source ^IValue solution))]
            [features (h-block-indexes block) 1.0])
        num-h-blocks (count h-blocks)
        block-counts (double-array num-h-blocks)
        feature-counts (to-array (repeatedly (count selected-features)
                                             #(double-array num-h-blocks)))]
    (debug-print (to-string selected-feature-vectors))
    (debug-print (to-string feature-matrix))
    (debug-printf "  select-features f-matrix is %d x %d\n"
                  (count feature-matrix) (when (not-empty feature-matrix)
                                           (count (first feature-matrix))))
    (debug-printf "  select-features f-counts is %d x %d\n"
                  (count feature-counts) (count (first feature-counts)))
    (doseq [row feature-matrix]
      (println "    " row))
    (doseq [[features block-idx c] observations]
      (debug-printf "  obs %.1f: %d %s\n" c block-idx (to-string features))
      (aadd block-counts block-idx c)
      (doseq [feature-idx features]
        (aadd (aget feature-counts feature-idx) block-idx c)))
    (let [scored-features (map (fn [f cvec]
                                 [f (Information/mutInfBinary block-counts cvec)])
                               selected-features feature-counts)
          sorted-features (sort-by (comp - second) scored-features)]
      (doall (take (config :search.feat.max-features) 
                   (apply concat (map first sorted-features)))))))

(reset! debug-print-active false)


(defn select-hypothesis-blocks [config h-prim-dist solution-sets]
  (let [max-blocks (config :search.hypo.max-blocks)
        solution-exprs (map (fn [^IValue x] (.source x)) (apply concat solution-sets))
        counts (count-subexpressions solution-exprs true)
        multi-use-exprs (filter (fn [[_ c]] (> c 1.0)) counts)
        selected-blocks (doall
                          (map (fn [[expr _]]
                                 (if (instance? IValue expr)
                                   expr
                                   (->Value (keyword (short-expr-string expr))
                                            expr
                                            (infer-type expr)
                                            (.denotation ^IValue (limited-eval expr))
                                            Double/NaN)))
                               (take max-blocks (sort-by (comp - val) multi-use-exprs))))]
    (distinct
      (concat (map se-value h-prim-dist) selected-blocks))))

(deftest select-hypothesis-blocks-test
  (let [h-prim-dist (uniform-primitive-dist [:I :+ :-] primitive-index)
        exprs (map (comp list typed-eval preprocess) '((C (B + I) 1)
                                                       (C (B + I) 2)
                                                       (C (B + I) 3)))
        correct-ids #{:+ :I (keyword "[B+I]") :-}
        config (make-mock-config {:search.hypo.max-blocks 3})
        blocks (select-hypothesis-blocks config h-prim-dist exprs)]
    (is (= correct-ids (set (map :id blocks))))))


(defrecord BlockDatum [properties labels]
  Datum
  (asFeatures [this] properties)
  (label [this]
   #_    (println "Warning: BlockDatum being treated as having single label")
    (first labels))
  (labels [this] labels))

#_(defn derive-properties [features contexts]
   (let [property-groups (group-by second
                                   (for [f features
                                         :let [values (map #(f %) contexts)]
                                         :when (apply not= values)] ; remove constant features
                                     [f values]))]
         ; TODO: Use property-groups to filter out redundant features.

     (transpose
       (for [group (vals property-groups)
             [f values] group]
         (for [value values]
           [f value])))))

#_(defn train-solution-model
   [^LinearClassifierFactory classifier-factory features contexts solution-sets]
   (let [property-sets (derive-properties features contexts)
         training-data (for [[solutions properties] (map vector solution-sets property-sets)
                             solution solutions
                             block (extract-values solution)]
                         (BlockDatum. properties block))]
     (.trainClassifier classifier-factory ^Collection training-data)))

(defn dump-model [^LinearClassifier model]
  (println "Model dump:")
  (.dump model))
  

(defn problem-properties [features problem]
  (filter (fn [f]
            (let [value (apply-pessimistically f problem)]
              (when (some? value)
                (.denotation value))))
          features))

(reset! debug-print-active true)

(defn train-hypothesis-model [config features problems solution-sets]
  (let [property-sets (map (partial problem-properties features) problems)
        classifier-factory (doto (LinearClassifierFactory.)
                             (.useConjugateGradientAscent)
                             (.setVerbose false)
                             (.setSigma (config :search.hypo.maxent.sigma)))
        training-data (for [[solution-set properties] (map vector solution-sets property-sets)
                            solution solution-set
                            block (extract-values solution)]
                        (BlockDatum. properties (list block)))
    
        #_ (debug-print "train-hypothesis-model")
        #_ (doseq [datum training-data]
            (debug-print (value-string (.label ^BlockDatum datum))
                     (mapv value-string (.asFeatures ^BlockDatum datum))))

        ;XXX inefficient expr.equals()
        model (.trainClassifier classifier-factory ^Collection training-data)]
    
    (when-debug-print-active
      (dump-model model))
    
    model))

(defn generate-hypotheses
  [config task-type h-prim-dist features ^LinearClassifier h-model problems]
  (let [prior-smoothing (config :search.hypo.prior-smoothing)]
    (assert (is (every? #(instance? ScoredExpr %) h-prim-dist)))
    (doall ; XXX
      (for [problem problems
            :let [properties (problem-properties features problem)
                  datum (BlockDatum. properties nil)
                  unnorm-dist (entries-to-vectors (.. h-model (scoresOf datum) entrySet))
                  _ (debug-print "generate-hypotheses unnorm-dist"
                     (to-string properties) "\n"
                     "" (map #(str (expr-string (.source ^IValue (first %))) " <" (second %) ">")
                             unnorm-dist))
                  h-block-dist (map (fn [[expr log-prob]] (ScoredExpr. expr log-prob))
                                    (normalize-log unnorm-dist (log 0.25)))
                  smoothed-dist (smooth-block-dists h-block-dist h-prim-dist prior-smoothing)
                  _ (debug-print "generate-hypotheses smoothed-dist"
                     (to-string properties) "\n"
                     "" (map se-string smoothed-dist))
                  scored-exprs (generate-exprs config "search.hypo" smoothed-dist task-type)]]
        (doall ; XXX
          (filter some?
                  (map (comp limited-eval se-expr) scored-exprs)))))))

(reset! debug-print-active false)

(defn image-of [^IValue f domain]
  (if (empty? domain) ; For this task type, f is actually a constant, not a function
    (.denotation f)
    (doall
      (for [x domain
            :let [y (apply-pessimistically f x)]
            :while (some? y)]
        (.denotation ^IValue y)))))

(defn select-solutions [config domain problems hypothesis-sets]
  (let [selector (selector-by-key (keyword (config :search.hypo.selector)))
        _ (doseq [hypotheses (take (count problems) hypothesis-sets)]
            (doall hypotheses)) ; XXX for easier debugging
        solvers (map (fn [problem hypotheses]
                       (let [problem-image (image-of problem domain)]
                         ; XXX inefficent .equals() on denotations?
                         (filter #(= (image-of % domain) problem-image)
                                 hypotheses)))
                     problems hypothesis-sets)]
    (doall (selector solvers))))

(defn make-test-trl-func [f]
  (->Value (str f) nil (make-type '(:int :int)) f 0))

(deftest select-solutions-test
  (let [config (make-mock-config {:search.hypo.selector :shortest})
        domain (map preprocess (range 3))
        problems (map make-test-trl-func (list identity inc))
        hypothesis-sets (repeat (map make-test-trl-func
                                     (list identity - (comp - -))))
        solvers (select-solutions config domain problems hypothesis-sets)
        solvers-ident (first solvers)
        solvers-inc (second solvers)]
    (is (= (count solvers-ident) 1))
    (is (= (count solvers-inc) 0))
    (is (= ((.denotation ^IValue (first solvers-ident)) 3) 3))))


(defn next-feature-block-dist [config features old-dist]
  (let [max-blocks (config :search.feat.max-blocks)
        gen-smoothing (config :search.feat.gen-smoothing)
        counts (count-subexpressions (map (fn [^IValue x] (.source x)) features) true)
        multi-use-exprs (filter (fn [[_ c]] (> c 1.0)) counts)
        [top-exprs top-expr-counts] (transpose (top-n val max-blocks multi-use-exprs))
        probs (normalize top-expr-counts 0.25) ; XXX Does this 0.25 really make sense?
        new-block-dist (filter #(> (val %) Double/NEGATIVE_INFINITY)
                               (smooth-log (zipmap top-exprs (map log probs))
                                           old-dist
                                           gen-smoothing))]
    (doall (for [[expr prob] (sort-by (comp - val) new-block-dist)]
             (ScoredExpr.
               (if (instance? IValue expr)
                 expr
                 (->Value (keyword (short-expr-string expr))
                          expr
                          (infer-type expr)
                          (.denotation ^IValue (limited-eval expr))
                          Double/NaN))
               prob)))))


(defn run-hal [config logger task-type task-domain problem-sets]
  (let [max-iterations (config :hal.iterations)
        feature-type (make-type (list task-type :bool))
        h-prim-dist (uniform-primitive-dist (config :search.hypo.primitives) primitive-index)
        f-prim-dist (uniform-primitive-dist (config :search.feat.primitives) primitive-index)
        initial-hypotheses (generate-initial-hypotheses config h-prim-dist task-type)]
    (loop [iteration                0
           remaining-problem-sets   (rest problem-sets)
           problems                 (first problem-sets)
           features                 nil
           h-blocks                 (map se-expr h-prim-dist)
           solution-sets            (select-solutions config
                                      task-domain problems initial-hypotheses)
           f-block-dist             f-prim-dist]
      (logger iteration problems solution-sets features h-blocks)
      (if (= iteration max-iterations)
        [problems solution-sets]
        (let [feature-candidates  (generate-feature-candidates config
                                    feature-type f-prim-dist f-block-dist)
              features'           (select-features config
                                    feature-candidates h-blocks problems solution-sets)
              h-blocks'           (select-hypothesis-blocks config 
                                    h-prim-dist solution-sets)
              h-model'            (train-hypothesis-model config 
                                    features' problems solution-sets)
              problems'           (first remaining-problem-sets)
              hypotheses'         (generate-hypotheses config
                                    task-type h-prim-dist features' h-model' problems')
              solution-sets'      (select-solutions config
                                    task-domain problems' hypotheses')
              f-block-dist'       (next-feature-block-dist config 
                                    features' f-block-dist)]
          (recur (inc iteration)
                 (rest remaining-problem-sets)
                 problems'
                 features'
                 h-blocks'
                 solution-sets'
                 f-block-dist'))))))
                                

(defn polynomial-problem-set [N max-order max-coeff seed]
  (let [poly-type (make-type '(:int :int))
        random (java.util.Random. seed)]
    (doall
      (repeatedly N
        (fn []
          (let [order (.nextInt random (inc max-order))
                coeffs (repeatedly (inc order) #(.nextInt random (inc max-coeff)))
                problem (Problem. coeffs
                               (fn [x]
                                 (apply + (for [[exp coeff] (map vector (range) coeffs)]
                                            (* coeff (apply * (repeat exp x)))))))]
            (->Value :poly nil poly-type problem Double/NaN)))))))

(defn compression-problem-set [N]
  (doall
    (map #(->Value (keyword (str %)) nil :int % Double/NaN) (range N))))

(defn run-hal-on-config-task [config]
  (dump-config config)
  (let [random-seeds (config :task.random-seeds)
        task-name (config :task.name)
        task-type (case task-name
                    "poly" (make-type '(:int :int))
                    "comp" (make-type :int))
        task-domain (map preprocess (config :task.domain))
        problem-sets (case task-name
                       "poly" (map #(polynomial-problem-set (config :task.problem-count)
                                                            (config :task.poly.order)
                                                            (config :task.poly.max-coeff)
                                                            %)
                                   random-seeds)
                       "comp" (repeat (compression-problem-set (config :task.problem-count))))
        logger (fn [iteration problems solution-sets features h-blocks]
                 (println "Iteration" iteration)
                 (doseq [[problem solutions] (transpose [problems solution-sets])]
                   (println (str "  " (:denotation problem) ": " 
                                 (string/join "; " (map #(to-string (.source ^IValue %))
                                                        solutions)))))
                 (println "Features:")
                 (doseq [feature features]
                   (println (str "  " (expr-string (:source feature)))))
                 (println "Blocks:")
                 (doseq [block h-blocks]
                   (println (expr-string block)))
                 (println (str "Solutions: " (count (filter not-empty solution-sets))))
                 (println))]
    (run-hal config logger task-type task-domain problem-sets)))

(defn -main [config-file]
  (doseq [config (make-configs config-file)]
    (time (run-hal-on-config-task config))))



(defn debug-type-node-expr-cache [type-desc]
  (let [node (.get ^HashMap (:node-index @latest-search-state) (make-type type-desc))
        s-exprs (sort-by (comp - se-score) (:exprs @(:expr-cache node)))]
    (println (:min-score node) (count s-exprs))
    (doseq [s-expr (concat (take 10 s-exprs) (list (last s-exprs)))]
      (println (se-score s-expr) (expr-string (se-expr s-expr))))))

(defn debug-type-node-app-entries [type-desc n]
  (let [node (.get ^HashMap (:node-index @latest-search-state)
                   (make-type type-desc))
        app-entries (:app-entries node)]
    (println (count app-entries))
    (doseq [app-entry (take n app-entries)]
      (println (:id (.router ^AppEntry app-entry))
               (to-string (:type (.lhs ^AppEntry app-entry))) ","
               (to-string (:type (.rhs ^AppEntry app-entry)))))))




























