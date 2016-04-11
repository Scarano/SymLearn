(ns genlearn.util
  (:require [clojure [test :refer :all]
                     [string :as string]])
  (:import [java.lang Math]
           [clojure.lang ISeq]
           [genlearn Util]))

(set! *warn-on-reflection* true)

(defn tuple [& fields]
  (let [size (count fields)
        array (object-array size)]
    (loop [i 0 fields-rem fields]
      (when (< i size)
        (aset array i (first fields-rem))
        (recur (inc i) (rest fields-rem))))
    #(nth array %)))
  
(defn pair [a b]
  (let [array (object-array 2)]
    (aset array 0 a)
    (aset array 1 b)
    #(nth array %)))

(comment
  (println "pair:" (map (pair \a \b) (range 2)))
  (println "tuple:" (map (tuple \a \b \c) (range 3)))
)

(defrecord Pair [first second])

(defn boolean? [x]
  (= (type x) Boolean))

(defn third [x]
  (first (next (next x))))

(defn while-realized [xs]
     (if (if (instance? clojure.lang.IPending xs)
             (and (realized? xs) (not-empty xs))
             (not-empty xs))
       (lazy-seq (cons (first xs) (while-realized (rest xs))))
       '()))

(defn count-above? [coll n]
  (if (seq? coll)
    (> (count (take (inc n) coll)) n)
    (> (count coll) n)))

(defn ^String to-string [& args]
  (apply str
    (for [arg args]
      (cond
        (nil? arg)
          "nil"
        (vector? arg)
          (str "[" (string/join ", " (map to-string arg)) "]")
        (seq? arg)
          (str "(" (string/join ", " (map to-string arg)) ")")
        (instance? clojure.lang.APersistentMap arg)
          (str "{" (string/join ", " (map (fn [[k v]]
                                            (str (to-string k) " " (to-string v)))
                                          arg)) "}")
        :else
          (str arg)))))

(defn ^String to-string-lim [limit & args]
  (apply str
    (for [arg args]
      (cond
        (nil? arg)
          "nil"
        (vector? arg)
          (str "[" (string/join ", " (map (partial to-string-lim limit) (take limit arg)))
               (if (count-above? arg limit) "...]" "]"))
        (seq? arg)
          (str "(" (string/join ", " (map (partial to-string-lim limit) (take limit arg)))
               (if (count-above? arg limit) "...)" ")"))
        (instance? clojure.lang.APersistentMap arg)
          (str "{" (string/join ", "
                                (map (fn [[k v]]
                                       (str (to-string-lim limit k) " " (to-string-lim limit v)))
                                     (take limit arg)))
               (if (count-above? arg limit) "...}" "}"))
        :else
          (str arg)))))

(defn print-string [& args]
  (println (apply (partial to-string-lim 30) args)))

(defn print-seq
  "This like (doseq [x arg] (println x)), except it isn't super-slow in the REPL"
  ([arg]
    (println (string/join "\n" arg)))
  ([arg & more]
    (print-seq arg)
    (println)
    (print-seq more)))


(defn fail 
  ([^String msg]
    (throw (RuntimeException. msg)))
  ([fmt & args]
    (throw (RuntimeException. ^String (apply format fmt args)))))


(def debug-print-active (atom true))

(defmacro when-debug-print-active [& args]
  (when @debug-print-active
    `(do ~@args)))

(defmacro debug-print [& args]
  (when @debug-print-active
    `(println (apply to-string (interpose " " (list ~@args))))))

(defmacro debug-printf [fmt & args]
  (when @debug-print-active
    `(printf ~fmt ~@args)))

(defmacro debug-pre-print [& args]
  (if @debug-print-active
    `(do
       (println (apply to-string (interpose " " (list ~@(butlast args)))))
       ~(last args))
    (last args)))

(defn debug-post-print-fn [& args]
  (println (apply to-string (interpose " " args)))
  (last args))
(defmacro debug-post-print [& args]
  (if debug-print-active
    `(debug-post-print-fn ~@args)
    (last args)))


(def ^:dynamic *trace-info* {})

(defn print-trace-fn
  "Only to be called from def-traceable-fn macro"
  ([test msg]
    (when test
      (println msg "in:")
      (doseq [call (reverse *trace-info*)]
        (println " " (to-string call)))))
  ([msg]
     (print-trace-fn true msg))
  ([]
     (print-trace-fn true "Trace")))
(defn return-traced-fn
  "Only to be called from def-traceable-fn macro"
  ([test msg result]
    (print-trace-fn test (to-string msg " [value = " result "]"))
    result)
  ([msg result]
    (return-traced-fn true msg result))
  ([result]
    (return-traced-fn true "Trace" result)))

(defmacro def-traceable-fn [active name & more]
  (if active
    `(do
      (defmacro ~'print-trace ([& args#] (cons 'print-trace-fn args#)))
      (defmacro ~'return-traced ([& args#] (cons 'return-traced-fn args#)))
      ~(if (vector? (first more))
         (let [[args & body] more]
           `(defn ~name ~args
              (binding [*trace-info* (cons (list (quote ~name) ~@args) *trace-info*)]
                ~@body)))
         `(defn ~name
            ~@(for [[args & body] more]
               `(~args
                  (binding [*trace-info* (cons (list (quote ~name) ~@args) *trace-info*)]
                    ~@body))))))
    `(do
       (defmacro ~'print-trace ([& args#] nil))
       (defmacro ~'return-traced ([& args#] (last args#)))
       (defn ~name ~@more))))

(defn ^double aadd [^doubles a ^long i ^double v]
  (aset a i (+ (aget a i) v)))

(defn argmax 
  ([f xs]
    (if (empty? xs)
      nil
      (argmax f (rest xs) (first xs) (f (first xs)))))
  ([f xs max-x max-val]
    (if (empty? xs)
      max-x
      (let [next-val (f (first xs))]
        (if (> next-val max-val)
          (recur f (rest xs) (first xs) next-val)
          (recur f (rest xs) max-x max-val))))))

(defn argmin [f xs] (argmax (comp - f) xs))

(deftest argmin-test
  (is (= (argmin count ["foo" "foobar" "me" "whatever"]) "me")))

(defn top-n [f n items]
  "Take the n items that have the highest (f item) value.
  Not the asymptotically fastest implementation, but no need to optimize at this point."
  (->> (sort-by (comp - f) items)
    (take n)))

(deftest top-n-test
  (is (= (set (top-n count 3 ["ab" "abc" "abcdef" "a" "abcd" "abcde"]))
         #{"abcd" "abcde" "abcdef"})))
  
(defn indexed-instance-unsync [^java.util.HashMap index key factory]
  "Thought this might be faster than indexed-instance, but it doesn't seem to be."
  (if-let [instance (.get index key)]
    instance
    (let [instance (factory)]
      (.put index key instance)
      instance)))

(defn indexed-instance [index key factory]
  (dosync
    (if-let [instance (@index key)]
      instance
      (let [instance (factory)]
        (commute index assoc key instance)
        instance))))

(defn map-leaves 
  ([f t]
    (map-leaves f t seq?))
  ([f t branch?]
    (if (branch? t)
      (map #(map-leaves f % branch?) t)
      (f t))))

;; From algo.monads
(defn lazy-concat [ss]
  (lazy-seq
    (when-let [s (seq ss)]
      (concat (first s) (lazy-concat (rest s))))))

(defn map-vals [f m]
  (persistent! (reduce (fn [m' [k v]] (assoc! m' k (f v))) (transient m) m)))

(deftest map-vals-test
  (is (= (map-vals inc {:a 1 :b 2})
         {:a 2 :b 3})))

(defn transpose
  "(transpose '((a1 a2) (b1 b2))) => ((a1 b1) (a2 b2))
  (transpose vector '((a1 a2) (b1 b2))) => ([a1 b1] [a2 b2])

  If row-constructor is not specified, row type will be whatever Clojure uses for arbitrary-
  arity function invocations, currently clojure.lang.ArraySeq. If a row-constructor is specified,
  it will be invoked on the \"columns\" of coll."
  ([coll]
    (if (empty? coll)
      '()
      (apply map (cons (fn [& col] col) coll))))
  ([row-constructor coll]
    (if (empty? coll)
      '()
      (apply map (cons row-constructor coll)))))

(defmacro def- [name decl]
  `(def ^{:private true} ~name ~decl))

(defmacro declare- [name]
  `(declare ^{:private true} ~name))

(defn benchmark
  ([f ^long iters] (benchmark f iters (/ iters 3)))
  ([f ^long iters ^long pre-iters]
    (dotimes [_ pre-iters] (f))
    (java.lang.System/gc)
    (time
      (do
        (dotimes [_ iters] (f))
        (java.lang.System/gc)))))

(defmacro defn-memo [name & body]
  `(def ~name (memoize (fn ~body))))

(defn unsynced-memoize [f]
  (let [memo (java.util.HashMap.)]
    (fn [& args]
      (let [result (.get memo args)]
        (if (some? result)
          result
          (let [result (apply f args)]
            (.put memo args result)
            result))))))

(defn entries-to-vectors [entries]
  (for [entry entries]
    [(.getKey ^java.util.Map$Entry entry) (.getValue ^java.util.Map$Entry entry)]))

;; Numerical stuff

(defn approx= [^double a ^double b]
  (< (Math/abs (- a b)) 0.000001))

(defn ^double exp [^double x] (Math/exp x))
(defn ^double log [^double x] (Math/log x))
(defn ^double log+
  ([^double a ^double b] (Util/logSum a b))
  ([^doubles arr]
    (let [len (int (alength arr))]
      (loop [i (int 0) sum (double 0)]
        (if (< i len)
          (recur (unchecked-inc i) (double (log+ sum (aget arr i))))
          sum)))))

(def log-2 (Math/log 2.0))

(defn ^double exp2 [^double x]
  (Math/exp (* x log-2)))

(defn normalize 
  ([densities]
    (if (coll? (first densities))
      (map vector (map first densities)
                  (normalize (map second densities)))
      (let [sum ^double (apply + densities)]
        (map (fn [^double x] (/ x sum)) densities))))
  ([densities ^double available-mass]
    (if (coll? (first densities))
      (map vector (map first densities)
                  (normalize (map second densities) available-mass))
      (map (fn [^double x] (* x available-mass)) (normalize densities)))))

(deftest normalize-test
  (let [a 1.0
        b 3.0
        a-norm 0.25
        b-norm 0.75
        a-norm2 0.5
        b-norm2 1.5]
    (is (= (normalize [a b])
           [a-norm b-norm]))
    (is (= (normalize [[:a a] [:b b]])
           [[:a a-norm] [:b b-norm]]))
    (is (= (normalize [a b] 2)
           [a-norm2 b-norm2]))
    (is (= (normalize [[:a a] [:b b]] 2)
           [[:a a-norm2] [:b b-norm2]]))))

(defn normalize-log 
  ([densities]
    (if (coll? (first densities))
      (map vector (map first densities)
                  (normalize-log (map second densities)))
      ;; XXX Optimization: write multi-arity log+ and use that instead of reduce
      (let [sum ^double (reduce log+ Double/NEGATIVE_INFINITY densities)]
        (map (fn [^double x] (- x sum)) densities))))
  ([densities ^double available-mass]
    (if (coll? (first densities))
      (map vector (map first densities)
                  (normalize-log (map second densities) available-mass))
      (map (fn [^double x] (+ x available-mass)) (normalize-log densities)))))

(deftest normalize-log-test
  (let [a (log 1.0)
        b (log 3.0)
        a-norm (log 0.25)
        b-norm (log 0.75)
        a-norm2 (log 0.5)
        b-norm2 (log 1.5)]
    (is (every? true? (map (fn [x y] (approx= x y))
                           (normalize-log [a b])
                           [a-norm b-norm])))
    (is (every? true? (map (fn [[k1 v1] [k2 v2]] (and (= k1 k2) (approx= v1 v2)))
                           (normalize-log [[:a a] [:b b]])
                           [[:a a-norm] [:b b-norm]])))
    (is (every? true? (map (fn [x y] (approx= x y))
                           (normalize-log [a b] (log 2))
                           [a-norm2 b-norm2])))
    (is (every? true? (map (fn [[k1 v1] [k2 v2]] (and (= k1 k2) (approx= v1 v2)))
                           (normalize-log [[:a a] [:b b]] (log 2))
                           [[:a a-norm2] [:b b-norm2]])))))

(defn smooth [d1 d2 lambda]
  (cond
    (= lambda 1.0)
      d1
    (= lambda 0.0)
      d2
    :else
      (let [d1' (zipmap (map first d1) (map #(* lambda (second %)) d1))
            d2' (zipmap (map first d2) (map #(* (- 1.0 lambda) (second %)) d2))]
        (merge-with (fn [d1-val d2-val] (+ d1-val d2-val)) d1' d2'))))

(deftest smooth-test
  (is (= (smooth {\a 0.5 \b 0.3 \c 0.2}
                 {\b 0.2 \c 0.3 \d 0.5} 0.6)
         {\a 0.3, \b 0.26, \c 0.24, \d 0.2})))

(defn smooth-log [d1 d2 lambda]
  (cond
    (= lambda 1.0)
      d1
    (= lambda 0.0)
      d2
    :else
      (let [logl (log lambda)
            log1-l (log (- 1.0 lambda))
            d1' (into {} (map (fn [[k v]] [k (+ logl v)]) d1))
            d2' (into {} (map (fn [[k v]] [k (+ log1-l v)]) d2))]
        (merge-with (fn [d1-val d2-val] (log+ d1-val d2-val)) d1' d2'))))

(deftest smooth-log-test
 (let [d1 {\a 0.5 \b 0.3 \c 0.2}
       d2 {\b 0.2 \c 0.3 \d 0.5}
       lambda 0.6
       smoothed (smooth d1 d2 lambda)
       log-d1 (map-vals log d1)
       log-d2 (map-vals log d2)
       log-smoothed (smooth-log log-d1 log-d2 lambda)
       exp-log-smoothed (map-vals exp log-smoothed)]
   (is (every? #(approx= (smoothed %) (exp-log-smoothed %))
               (keys (merge smoothed exp-log-smoothed))))))


    
;; Taken from deprecated clojure.contrib, and written by Stephen C. Gilardi

(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))

