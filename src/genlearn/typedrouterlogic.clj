(ns genlearn.typedrouterlogic
  (:require (clojure [test :refer :all]
                     [string :as string])
            (genlearn [util :refer :all]
                      [curry :refer :all]
                      [types :refer :all]))
  (:import (clojure.lang ArityException)
           (java.lang Double)
           (genlearn IType
                     IExpr
                     IValue
                     IApp
                     IScored
                     ComputationLimitExceededException)))


(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)


(def ^:const computation-limit 30)

(defn routing-combinator? [x]
  (and (keyword? x)
       (let [s (name x)]
         (and (> (.length s) 0)
              (every? #{\B \C \S} s)))))

(declare make-app)

(defrecord Value [id ^IExpr source ^IType type denotation score]

  IValue
  
  (id [this] id)
  (^IExpr source [this] (or source this))
  (^IType type [this] type)
  (denotation [this] denotation)
  (^double score [this]
    (or score Double/NaN))
  
  (^IValue apply [this ^IValue arg]
    (.apply this arg true (make-app this arg)))
  (^IValue apply [this ^IValue arg ^boolean populate-type ^IExpr source]
    (let [new-denotation (denotation (.denotation arg))]
      (when (some? new-denotation)
        (Value. nil
                source
                (if populate-type (returned-type type (.type arg)) (atomic-type :?))
                (denotation (.denotation arg))
                Double/NaN))))
  
  clojure.lang.IFn

  (invoke [this arg]
    (.apply this arg))

;  (applyTo [this args]
;    (if (= (count args) 1)
;      (throw (ArityException. (count args)
;               (str "Attempt to apply value <" (.toString this) 
;                    "> on non-singleton argument list")))))
  
  Object
  (toString [this]
    (str (cond
           (some? id)
             (name id)
           (and (fn? denotation) (some? source))
             (str "Fn " (.toString source))
           (fn? denotation)
             (to-string "Fn : " type)
           :else
             (to-string-lim 30 denotation))
         (if (or (Double/isNaN score) (zero? score)) "" (format " <%.3f>" score)))))

(defn value-with-score [^IValue value new-score]
  (Value. (.id value) (.source value) (.type value) (.denotation value) new-score))

;(defrecord Primitive [id type denotation]
;  Value
;  
;  Object ;; XXX do i need to put Object here, instead of Value, to override toString?
;  (toString [this]
;    (str (name id) " : " (type-string type))))

(defn make-primitive [id type-desc denotation]
  (Value. id nil (make-type type-desc) denotation 0.0))

#_(defn get-expr [^IValue value]
   "IValue.expr can mean both the name and the expression that evaluated to the value. Until
  I fix that ill-advised overloading, this function can be used to get the actual expression."
   (if (instance? IApp (.expr value))
     (.expr value)
     value))

(defn router-lhs-args [^String router]
  (if (instance? IValue router)
    (recur (name (.id ^IValue router)))
    (filter #(#{\S \C} (.charAt router %)) (range (.length router)))))
(defn router-rhs-args [^String router]
  (if (instance? IValue router)
    (recur (name (.id ^IValue router)))
    (filter #(#{\S \B} (.charAt router %)) (range (.length router)))))

(defn router-type [^String router]
  (let [lhs-type-vars (router-lhs-args router)
        rhs-type-vars (router-rhs-args router)]
    (make-type (concat
                 (list (concat lhs-type-vars '(-2 -1))
                       (concat rhs-type-vars '(-2)))
                 (range (.length router))
                 '(-1)))))

(defn router-code-inner [^String rname level]
  (if (= level (.length rname))
    (let [lhs-vars (map #(symbol (str "x" %))
                        (filter #(#{\S \C} (.charAt rname %)) (range (.length rname))))
          rhs-vars (map #(symbol (str "x" %))
                        (filter #(#{\S \B} (.charAt rname %)) (range (.length rname))))
          apply-rec (fn apply-rec [f args]
                      (if (empty? args) f (list (apply-rec f (rest args)) (first args))))
          lhs (apply-rec 'f (reverse lhs-vars))
          rhs (apply-rec 'g (reverse rhs-vars))]
      (list lhs rhs))
    (list 'fn [(symbol (str "x" level))]
      (router-code-inner rname (inc level)))))
(defn router-code [^String rname]
  (list 'fn ['f]
    (list 'fn ['g]
      (router-code-inner rname 0))))
(defn router-denotation [^String rname]
  (eval (router-code rname)))
(defn make-router [^String rname]
  (Value. (keyword (if (= rname "") "I" rname)) nil (router-type rname) (router-denotation rname)
          0.0))
(def make-router-mem (memoize make-router))

(defn router-arity [^IValue router]
  (if (= (.id router) :I)
    0
    (.length ^String (name (.id router)))))

(defn enumerate-routers [arity]
  (if (zero? arity)
    (list "")
    (let [short-routers (enumerate-routers (dec arity))]
      (concat
        (map #(str "B" %) short-routers)
        (map #(str "C" %) short-routers)
        (map #(str "S" %) short-routers)))))

(defn safe-divide [^long a]
  (fn [^long b]
    (if (= b 0)
      nil
      (/ a b))))

(defn safe-count [coll]
  (let [c (count (take (inc computation-limit) coll))]
    (if (> c computation-limit)
      (throw (ComputationLimitExceededException.))
      c)))

(def primitive-library (list
    (make-primitive :I '(a a) identity)
    
;    (make-primitive :B '((b c) (a b) a c) (fn [f]
;                                            (fn [g]
;                                              (fn [x]
;                                                (f (g x))))))
;    (make-primitive :C '((a b c) b a c) (fn [f]
;                                          (fn [g]
;                                            (fn [x]
;                                              ((f x) g)))))
;    (make-primitive :S '((a b c) (a b) a c) (fn [f]
;                                              (fn [g]
;                                                (fn [x]
;                                                  ((f x) (g x))))))
    
    (make-primitive :T :bool true)
    (make-primitive :F :bool false)
    
    (make-primitive :not '(:bool :bool) not)
    (make-primitive :and '(:bool :bool :bool) #(fn [b] (and % b)))
    (make-primitive :or '(:bool :bool :bool) #(fn [b] (or % b)))
    (make-primitive :if '(:bool a a a) (fn [test]
                                         (fn [then]
                                           (fn [else]
                                             (if test then else)))))
    
    (make-primitive :0 :int 0)
    (make-primitive :1 :int 1)

    (make-primitive :neg '(:int :int) -)
    (make-primitive :inc '(:int :int) inc)
    (make-primitive :+ '(:int :int :int) (curry2 +))
    (make-primitive :- '(:int :int :int) (curry2 -))
    (make-primitive :* '(:int :int :int) (curry2 *))
    (make-primitive :/ '(:int :int :int) safe-divide)
    (make-primitive := '(:int :int :bool) (curry2 =))
    (make-primitive :> '(:int :int :bool) (curry2 >))
    
    (make-primitive :empty '[a] '())
    (make-primitive :singleton '(a [a]) list)
    (make-primitive :cons '(a [a] [a]) (curry2 cons))
    (make-primitive :first '([a] a) #(when (not-empty %) (first %)))
    (make-primitive :rest '([a] [a]) #(when (not-empty %) (rest %)))
    (make-primitive :empty? '([a] :bool) empty?)
    (make-primitive :count '([a] :int) safe-count)
    (make-primitive :range '(:int :int [:int]) (curry2 range))

    #_(
    ; List operations eagerly evaluate first <computation-limit> elements to
    ; ensure that the list is no longer than that. This is lame; I really should
    ; just be careful not to evaluate too many elements of computation results.
    (make-primitive :map '((a b) [a] [b]) (fn [f]
                                            (fn [coll]
                                              (safe-count coll)
                                              (map f coll))))
    (make-primitive :filter '((a :bool) [a] [a]) (fn [f]
                                                   (fn [coll]
                                                     (safe-count coll)
                                                     (filter f coll))))
    (make-primitive :mapcat '((a [b]) [a] [b]) (fn [f]
                                                 (fn [coll]
                                                   (safe-count coll)
                                                   (mapcat f coll))))
    )
    (make-primitive :map '((a b) [a] [b]) (curry2 map))
    (make-primitive :zip '((a b c) [a] [b] [c]) (fn [f]
                                                  (fn [coll1]
                                                    (fn [coll2]
                                                      (map #((f %1) %2) coll1 coll2)))))
    (make-primitive :filter '((a :bool) [a] [a]) (curry2 filter))
    (make-primitive :mapcat '((a [b]) [a] [b]) (curry2 mapcat))
    (make-primitive :fold '((a b a) a [b] a) (fn [f]
                                               (fn [val]
                                                 (fn [coll]
                                                   (safe-count coll)
                                                   (reduce #((f %1) %2) val coll)))))
    (make-primitive :unfold '((a a) a [a]) (curry2 iterate))
    
    ; intended for features, not solutions
    (make-primitive :sample8 '((:int b) [b]) (fn [f]
                                               (map f (range 8))))))

(def primitive-routers
  (map (fn [rname] (Value. (keyword rname) nil (router-type rname) (router-denotation rname) 0.0))
       (mapcat #(enumerate-routers %) (range 1 5))))

(defn null-router? [^IValue r] (or (nil? r) (= (.id r) :I)))

(defrecord App [^IValue router ^IExpr lhs ^IExpr rhs ^double score]
  IApp
  (^IValue router [this] router)
  (^IExpr lhs [this] lhs)
  (^IExpr rhs [this] rhs)
  (^double score [this] (or score Double/NaN))
    
;  (^IApp eval [this]
;    (let [lhs-val (.eval lhs)
;          rhs-val (.eval rhs)]
;      (-> (if (some? router) (.apply router lhs-val) lhs-val)
;        (.apply rhs-val))))
  
  Object
  (^String toString [this]
    (str "("
         (apply to-string (interpose " " (keep identity (list router lhs rhs))))
         ")"
         (if (or (Double/isNaN score) (zero? score)) "" (format " <%.3f>" score)))))

(defn make-app
  ([^IExpr lhs ^IExpr rhs] 
    (App. (make-router-mem "") lhs rhs (+ (.score lhs) (.score rhs))))
  ([^IValue router ^IExpr lhs ^IExpr rhs]
    (App. router lhs rhs (+ (.score lhs) (.score rhs))))
  ([^IValue router ^IExpr lhs ^IExpr rhs ^double base-score]
    (App. router lhs rhs (+ base-score (.score lhs) (.score rhs)))))

(declare expr-string)

(defn infer-type [^IExpr expr]
  (if (instance? IValue expr)
    (.type expr)
    (let [app ^IApp expr
          lhs-type (infer-type (.lhs app))
          rhs-type (infer-type (.rhs app))]
      (when (and (some? lhs-type) (some? rhs-type))
        (let [ret-type (if (null-router? (.router app))
                         (returned-type lhs-type rhs-type)
                         (let [lhs-type' (returned-type (.type (.router app)) lhs-type)]
                           (when (some? lhs-type')
                             (returned-type lhs-type' rhs-type))))]
          (if (some? ret-type)
            ret-type
            (println "! infer-type WARNING invalid expr:"
                     (expr-string expr) ;":" (to-string (.type expr))
                     "; lhs :" (to-string lhs-type) ", rhs :" (to-string rhs-type))))))))
      
(defn guess-type [denotation]
  (cond
    (integer? denotation) (atomic-type :int)
    (boolean? denotation) (atomic-type :bool)
    (nil? denotation) (make-type '[a])
    (seq? denotation) (list-type (if (empty? denotation) (var-type 1)
                                                         (guess-type (first denotation))))
    (fn? denotation) (function-type (var-type 1) (var-type 2))
    :else (throw (Exception. (str "Can't guess type of <" denotation ">")))))

(defn expr-string [expr]
  (if (instance? IValue expr)
    (str (assoc expr :score Double/NaN))
    (let [expr ^IApp expr]
      (str (if (null-router? (.router expr)) ""
                                             (str (name (.id (.router expr))) " "))
           (if (and (instance? IApp (.lhs expr))
                    (or (not (null-router? (.router expr)))
                        (not (null-router? (.router ^IApp (.lhs expr))))))
             (str "(" (expr-string (.lhs expr)) ")")
             (expr-string (.lhs expr)))
           " "
           (if (instance? IApp (.rhs expr)) (str "(" (expr-string (.rhs expr)) ")")
                                            (expr-string (.rhs expr)))))))

(defn- var-name [v]
  (if (and (>= v 0) (<= v 25))
    (str (char (+ v 97)))
    (str v)))
#_(defn lambda-ize [expr]
   "Make human-readable lambda expression for provided combinatory logic expression"
   ([expr] (lambda-ize expr [] 0))
   ([expr bound-vars next-var]
   (if (instance? IValue expr)
     (expr-string expr)
     (let [app ^IApp expr
           {:keys [router lhs rhs]} app
           bound-vars ()]
       ()))))

; key-fn is applied to each item to get the key
; val-fn is applied to (item, current-value-of-key-in-index)
;  The latter starts with the value nil
;  (This fn may replace current value of key, or append to a list, etc.)
(defn make-index [key-fn val-fn items]
  (reduce
    (fn [acc item] 
      (let [k (key-fn item)]
        (assoc acc k (val-fn item (acc k)))))
    {} items))

(def primitive-index
  (make-index :id (fn [item prev-val] item) (concat primitive-library primitive-routers)))

(defn preprocess [quoted-expr]
  (cond
    (symbol? quoted-expr)
      (or (primitive-index (keyword (str quoted-expr)))
          (throw (Exception. (str "Could not find primitive: " quoted-expr))))
    (number? quoted-expr)
      (or (primitive-index (keyword (str quoted-expr)))
          (Value. nil nil (guess-type quoted-expr) quoted-expr 0.0))
    (and (seq? quoted-expr) (= (count quoted-expr) 2))
      (make-app (preprocess (first quoted-expr))
                (preprocess (second quoted-expr)))
    (and (seq? quoted-expr) (= (count quoted-expr) 3))
      (make-app (make-router-mem (str (first quoted-expr)))
                (preprocess (second quoted-expr))
                (preprocess (third quoted-expr)))
    :else
      (Value. quoted-expr nil (guess-type quoted-expr) quoted-expr 0.0)))

(defn reduce-expr [f x ^IExpr expr]
  "Like reduce, but traversers every node in expression, including branches, pre-order"
  (let [x' (f x expr)]
    (if (instance? IApp expr)
      (let [app ^IApp expr
            x'' (reduce-expr f x' (.lhs app))]
        (recur f x'' (.rhs app)))
      x')))

(defn reduce-expr-including-primitives [f x ^IExpr expr]
  "Like reduce-expr, except it goes inside the :source of each non-atomic primitive."
  (if (instance? IValue expr)
    (let [source-expr (.source ^IValue expr)]
      (if (instance? IApp source-expr)
        (recur f x source-expr)
        (f x expr)))
    (let [x' (f x expr)
          x'' (reduce-expr f x' (.lhs ^IApp expr))]
      (recur f x'' (.rhs ^IApp expr)))))

(deftest reduce-expr-test
  (is (= (reduce-expr (fn [x expr] (if (instance? IValue expr) (inc x) x))
                      0
                      (preprocess '((* 1) ((+ 0) 1))))
         5))
  (is (= (reduce-expr-including-primitives
           (fn [x expr] (if (instance? IValue expr) (inc x) x))
           0
           (App. (make-router "")
             (App. (make-router "")
               (primitive-index :*)
               (primitive-index :1) 0.0)
             (Value. :+01 (preprocess '((+ 0) 1)) (make-type :int) 1 0.0) 0.0))
         5)))

(defn extract-values [^IExpr expr]
  "Traverse expr and list all leaves (IValues)"
  (persistent!
    (reduce-expr
      (fn [values node]
        (cond-> values
          (instance? IValue node) (conj! node)))
      (transient [])
      expr)))

(defn expand-primitives [^IExpr expr]
  (if (instance? IValue expr)
    (if (instance? IApp (.source ^IValue expr))
      (recur (.source ^IValue expr))
      expr)
    (let [app ^IApp expr
          lhs (expand-primitives (.lhs app))
          rhs (expand-primitives (.rhs app))]
      (App. (.router app) lhs rhs (.score app)))))

(deftest expand-primitives-test
  (is (= (expand-primitives (make-app
                              (make-app
                                (primitive-index :*)
                                (primitive-index :1))
                              (Value. :+01 (preprocess '((+ 0) 1)) (make-type :int) 1 0.0)))
         (preprocess '((* 1) ((+ 0) 1)))))) 


(defn filter-args [desired comb args]
  (mapcat (fn [c a] (if (desired c) (list a) '())) comb args))
(def filter-args-left (partial filter-args #{\S \C}))
(def filter-args-right (partial filter-args #{\S \B}))

(def debug-print-level (atom 0))
(defn- spaces [n]
  (apply str (repeat n " ")))
(defn ^IValue typed-eval
  "Evaluate IExpr, percolating type information to the returned value."
  ([^IExpr exp]
    (cond
      (instance? IValue exp)
        exp
      (instance? IApp exp)
        (let [app ^IApp exp
              lhs (typed-eval (.lhs app))
              rhs (typed-eval (.rhs app))
              router (.router app)
              lhs (if (some? router) (.apply router lhs) lhs)]
          (.apply ^IValue lhs rhs true exp))
      :else
        (throw (Exception. (to-string "invalid exp: " exp)))))
  ([exp args]
    (if (empty? args)
      (typed-eval exp)
      (let [arg (first args)]
        (recur (make-app exp (first args)) (rest args))))))

(defn ^IValue fast-eval
  "Evaluate IExpr, throwing out type information to save time."
  ([^IExpr exp]
    (cond
      (instance? IValue exp)
        exp
      (instance? IApp exp)
        (let [app ^IApp exp
              lhs (fast-eval (.lhs app))
              rhs (fast-eval (.rhs app))
              router (.router app)
              lhs (if (some? router) (.apply router lhs) lhs)]
          (.apply ^IValue lhs rhs false exp))
      :else
        (throw (Exception. (to-string "invalid exp: " exp)))))
  ([exp args]
    (if (empty? args)
      (fast-eval exp)
      (let [arg (first args)]
        (recur (make-app exp (first args)) (rest args))))))

(defn ^IValue limited-eval 
  "Evaluate IExpr, throwing out type information. Halts and returns nil if number of
   computations exceeds max-apps."
  ([^IExpr exp ^long max-apps]
    (cond
      (instance? IValue exp)
        exp
      (not (pos? max-apps))
        nil
      (instance? IApp exp)
        (let [app ^IApp exp
              lhs (limited-eval (.lhs app) (dec max-apps))
              rhs (when (some? lhs) (limited-eval (.rhs app) (dec max-apps)))
              router ^IValue (.router app)
              lhs (when (some? lhs)
                    (if (null-router? router) lhs (.apply router lhs)))]
          (when (and (some? lhs) (some? rhs))
            (try
              (.apply ^IValue lhs rhs false exp)
              (catch Exception e
                (println "Exception caught in limited-eval:")
                (println " " (str exp))
                (println "  Expr:" (expr-string exp))
                (println "  type:" (type-string (infer-type exp)))
                (println "  LHS:" (expr-string lhs) ":" (type-string (infer-type lhs)))
                (println "  RHS:" (expr-string rhs) ":" (type-string (infer-type rhs)))
                (throw e)))))
      :else
        (throw (Exception. (to-string-lim 1000 "invalid exp: " exp)))))
  ([exp args ^long max-apps]
    (if (empty? args)
      (limited-eval exp)
      (let [arg (first args)]
        (recur (make-app exp (first args)) (rest args) max-apps))))
  ([exp]
    (limited-eval exp computation-limit)))

(def example-inc
  (preprocess
    '(C (B + I) 1)))
(def example-inc-with-lambda
  (preprocess
    '(C (BC I I) (+ 1))))
(def example-inc-with-lambda2
  (preprocess
    '(C (BC I I) (C (B + I) 1))))
(def example-*f11x
  (preprocess
    '(CC (CBC (BC I (C (C I 1) 1)) I) (BC (B * I) I))))
;    '(CC (CBC (BC I (C (C I 1) 1)) I) *)))
(def example-x2+y
  (preprocess
    '(CB (B + (S (B * I) I)) I)))
(def example-fact
  (preprocess
    '(S (B (fold *) I) (B (range 1) I))))

(deftest typed-eval-test
  (let [verbose false]
    (doseq [[example raw-args expect] [
                                       [example-inc '(10) #(= % 11)]
                                       [example-inc-with-lambda '() #(= (% 10) 11)]
                                       [example-inc-with-lambda '(10) #(= % 11)]
                                       [example-inc-with-lambda2 '(10) #(= % 11)]
                                       [example-*f11x '(+ 10) #(= % 20)]
                                       [example-x2+y '(10 1000) #(= % 1100)]
                                       [example-fact '(5) #(= % 120)]
                                      ]]
      (when verbose
        (println)
        (println (to-string "Applying " example " to " raw-args)))
      (let [args (map preprocess raw-args)
            result (typed-eval example args)]
        (is (do
              (when verbose
                (println (to-string result)))
              (expect (:denotation result))))))))

(defn benchmarks []
  (println)
  (time 
    (doseq [[example args] [
	                           [example-inc '(10)]
	                           [example-inc-with-lambda '()]
	                           [example-inc-with-lambda '(10)]
	                           [example-inc-with-lambda2 '(10)]
                             [example-*f11x '(+ 10)]
                             [example-x2+y '(10 1000)]
                             [example-fact '(5)]
	        ]]
      (println (to-string example " " args))
      (benchmark #(typed-eval example (map preprocess args)) 10000))))

(defn -main [& args]
;  (run-tests 'genlearn.typedrouterlogic))
  (benchmarks))

(defn gen-arg-name [arg-num]
  (str (if (>= arg-num 26) (gen-arg-name (/ arg-num 26)) "")
       (char (+ 97 (mod arg-num 26)))))

(defn lambda-ize
  "Turn router-logic expression into lambda-calculus expression using Clojure's (fn [arg] ...)
  syntax."
  ([expr]
    (lambda-ize expr [] 0))
  ([^IExpr expr arg-names next-arg-num]
    (let [is-app (instance? IApp expr)
          app ^IApp expr
          value ^IValue expr
          arity (if is-app (router-arity (.router app)) 0)
          deficit (- arity (count arg-names))]
      (cond
        (pos? deficit)
          (let [arg-name (symbol (gen-arg-name next-arg-num))]
            (list 'fn (vector arg-name)
                  (lambda-ize expr (conj arg-names arg-name) (inc next-arg-num))))
        (and (= deficit -1) (not is-app) (= (.id value) :I))
          (first arg-names)
        (neg? deficit)
          (list (lambda-ize expr (subvec arg-names 0 (dec (count arg-names))) next-arg-num)
                (last arg-names))
        :else ; arity = number of args
          (if (not is-app)
            value
            (let [lhs-args (vec (map arg-names (router-lhs-args (.router app))))
                  rhs-args (vec (map arg-names (router-rhs-args (.router app))))]
              (list (lambda-ize (.lhs app) lhs-args next-arg-num)
                    (lambda-ize (.rhs app) rhs-args next-arg-num))))))))

(defn make-readable [expr]
  "Turn router-logic expression into human-readable lambda calculus expression by calling
  lambda-ize, and then turning the Values at the leaves into their IDs or denotations."
  (map-leaves #(if (instance? IValue %)
                 (if-let [id (.id ^IValue %)]
                   (symbol (name id))
                   (.denotation ^IValue %))
                 %)
              (lambda-ize expr)))

(deftest lambda-ize-test
  (doseq [[expr expected] '(
     [(CCBB I I)
        (fn [a] (fn [b] (fn [c] (fn [d] ((a b) (c d))))))]
     [(CCBB 101 102)
        (fn [a] (fn [b] (fn [c] (fn [d] (((101 a) b) ((102 c) d))))))]
     [(CC (C 101 102) 103)
        (fn [a] (fn [b] ((((101 a) 102) b) 103)))]
     [(C (CC 101 102) 103)
        (fn [a] ((fn [b] (((101 a) b) 102)) 103))]
     [(S * I)
        (fn [a] ((* a) a))]
     [(CB (B + (S * I)) I)
        (fn [a] (fn [b] ((+ ((* a) a)) b)))]
     [(((BC I I) (S * I)) (CB I I))
        (((fn [a] (fn [b] (b a))) (fn [a] ((* a) a))) (fn [a] (fn [b] (a b))))]
                          )]
;    (println (make-readable (preprocess expr)))
;    (println expected)
    (is (= (make-readable (preprocess expr)) expected))))

; These two functiosn need mroe work -- it's not *quite* that easy.
#_(defn compile-to-clojure [expr]
   (map-leaves #(if (instance? IValue %) (.denotation ^IValue %) %)
               (lambda-ize expr)))

#_(defn compile-to-fn [expr]
   (eval (compile-to-clojure expr)))

















