(ns genlearn.types
  (:require (genlearn [util :refer :all])
            (clojure [test :refer :all]
                     [string :as string]))
  (:import [clojure.lang Keyword]
           [genlearn IType]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(declare type-string)

(defn f [^IType t] t)

(defrecord Type [^clojure.lang.Keyword type var ^IType lhs ^IType rhs]
  
  IType
  (^clojure.lang.Keyword type [this] type)
  (^int var [this] var)
  (^IType member [this] lhs)
  (^IType lhs [this] lhs)
  (^IType rhs [this] rhs)
  
  Object
  (toString [this]
    (type-string this)))

(defn ^IType atomic-type [name]
  (Type. name 0 nil nil))
(defn ^IType var-type [var-index]
  (Type. :Var var-index nil nil))
(defn ^IType function-type [^IType param ^IType ret]
  (Type. :Fn  0 param ret))
(defn ^IType list-type [^IType member]
  (Type. :List  0 member nil))

(defn ^IType make-type-unnormalized [desc]
  (cond
    (instance? IType desc)
      desc
    (keyword? desc)
      (Type. desc 0 nil nil)
    (symbol? desc)
      (Type. :Var (- (int (first (str desc))) 96) nil nil)
    (integer? desc)
      (Type. :Var desc nil nil)
    (vector? desc)
      (Type. :List 0 (make-type-unnormalized (first desc)) nil)
    (seq? desc)
      (if (empty? (rest desc))
        (make-type-unnormalized (first desc))
        (Type. :Fn 0 (make-type-unnormalized (first desc))
                     (make-type-unnormalized (rest desc))))
    :else
      (throw (Exception. (to-string "Can't make type from " desc)))))

(declare normalize-vars)
(defn ^IType make-type [desc]
  (normalize-vars (make-type-unnormalized desc)))

(defn var-type? [^IType t]
  (= (.type t) :Var))

(defn list-type? [^IType t]
  (= (.type t) :List))

(defn function-type? [^IType t]
  (= (.type t) :Fn))

#_(defn return-type [^IType t]
   (if-not (function-type? t)
     t
     (recur (.rhs t))))

(defn identity-type? [^IType t]
  (and (function-type? t)
       (var-type? (.lhs t))
       (var-type? (.rhs t))
       (= (.var (.lhs t)) (.var (.rhs t)))))

(defn type-string [^IType t]
  (cond
    (var-type? t)
      (let [v (.var t)]
        (if (and (>= v 1) (<= v 26))
          (str (char (+ v 96)))
          (str v)))
    (function-type? t)
      (let [lhs (type-string (.lhs t))
            rhs (type-string (.rhs t))]
        (if (function-type? (.lhs t))
          (str "(" lhs ") -> " rhs)
          (str lhs " -> " rhs)))
    (list-type? t)
      (str "[" (type-string (.member t)) "]")
    :else
      (name (.type t))))
(defn type-string<> [^IType t]
  (str "<" (type-string t) ">"))


(defn find-arity [^IType t]
  (loop [acc 0 t' t]
    (if (function-type? t')
      (recur (inc acc) (.rhs t'))
      acc)))

(deftest find-arity-test
  (doseq [[t arity] [[(make-type '[a]) 0]
                     [(make-type '((a a a) a)) 1]
                     [(make-type '(:int (:int :int) (:int :int) :int)) 3]]]
    (is (= arity (find-arity t)))))
  
(defn normalized-varmap
  ([^IType t] (normalized-varmap t {}))
  ([^IType t varmap]
    (cond
      (var-type? t)
        (if (contains? varmap (.var t))
          varmap
          (assoc varmap (.var t) (inc (count varmap))))
      (function-type? t)
        (let [varmap' (normalized-varmap (.lhs t) varmap)]
          (recur (.rhs t) varmap'))
      (list-type? t)
        (recur (.member t) varmap)
      :else
        varmap)))

(defn ^IType map-type [^IType t f]
  (f (case (.type t)
       :Fn
         (let [lhs (map-type (.lhs t) f)
               rhs (map-type (.rhs t) f)]
           (if (and (identical? (.lhs t) lhs) (identical? (.rhs t) rhs))
             t
             (Type. :Fn 0 lhs rhs)))
       :List
         (let [member (map-type (.member t) f)]
              (if (identical? (.member t) member)
                t
                (Type. :List 0 member nil)))
       t)))

(defn ^IType map-type-pessimistic [^IType t f]
  (f (case (.type t)
       :Fn
         (Type. :Fn 0 (map-type-pessimistic (.lhs t) f)
                      (map-type-pessimistic (.rhs t) f))
       :List
         (Type. :List 0 (map-type-pessimistic (.member t) f) nil)
       t)))

; re-implement using map-type?
(defn ^IType normalize-vars
  ([^IType t] (normalize-vars t (normalized-varmap t)))
  ([^IType t varmap]
    (case (.type t)
      :Var
        (make-type-unnormalized (varmap (.var t)))
      :Fn
        (Type. :Fn 0 (normalize-vars (.lhs t) varmap) (normalize-vars (.rhs t) varmap))
      :List
        (Type. :List 0 (normalize-vars (.member t) varmap) nil)
      t)))

(defn ^IType normalize-vars
  ([^IType t] (normalize-vars t (normalized-varmap t)))
  ([^IType t varmap]
    (map-type t
      (fn [^IType t]
        (if (var-type? t)
          (let [v (.var t)
                v' (varmap v)]
            (if (= v v')
              t
              (var-type v')))
          t)))))

(deftest normalized-vars-test
  (let [t (make-type-unnormalized '((:bool a) [b] :int))
        t' (make-type-unnormalized '((:bool d) [c] :int))]
    (is (= (normalize-vars t') t)))
  (let [t (make-type '(a b [a]))
        t' (normalize-vars t)]
    (is (identical? t t'))))

(defn ^IType negate-vars [^IType t]
  (map-type-pessimistic t
    (fn [^IType t] (if (var-type? t) (Type. :Var (- (.var t)) nil nil) t))))

(deftest negate-vars-test
  (let [t (make-type-unnormalized '((:bool 1) [2] :int))
        t' (make-type-unnormalized '((:bool -1) [-2] :int))]
    (is (= (negate-vars t') t))))

(defn- occurs-in [v ^IType t]
  (case (.type t)
    :Var   (= v (.var t))
    :Fn    (if (occurs-in v (.lhs t)) true (recur v (.rhs t)))
    :List  (recur v (.member t))
           false))

(defn substitute 
  ([v ^IType b ^IType t]
    (map-type t
      (fn [^IType t]
        (if (var-type? t)
          (if (= (.var t) v) b t)
          t))))
  ([bindings ^IType t]
    (map-type t
      (fn [^IType t]
        (if (var-type? t)
          (let [b (bindings (.var t))]
            (if (nil? b) t b))
          t)))))
          
(deftest subtitute-test
  ;; single-binding (3 arg)
  (is (= (substitute 1 (make-type-unnormalized '(:int b)) 
                       (make-type-unnormalized '(a [(c :int)] [a])))
         (make-type-unnormalized '((:int b) [(c :int)] [(:int b)]))))

  ;; all bindings in map (2 arg)
  (let [t (make-type-unnormalized '((:bool a) [b] :int))
        bindings {1 (make-type-unnormalized '(c :str))
                  2 (make-type-unnormalized 'd)}]
    (is (= (substitute bindings t)
           (make-type-unnormalized '((:bool c :str) [d] :int))))))

(defn- add-binding [bindings v ^IType t]
  (-> (map-vals (partial substitute v t) bindings)
      (assoc v t)))

(deftest add-binding-test
  (is (= (-> {}
           (add-binding 1 (make-type-unnormalized '(a b)))
           (add-binding 2 (make-type-unnormalized '[:int])))
         {1 (make-type-unnormalized '(a [:int]))
          2 (make-type-unnormalized '[:int])})))

(defn binding-string [[v b]]
  (str "[" (type-string (Type. :Var v nil nil)) " => " (type-string b) "]"))
(defn bindings-string [bindings]
  (string/join " " (map binding-string bindings)))

(def unify-debug false)

(defn unify
  ([^IType t ^IType u] 
#_    (when (= 0 (bit-and 0xfff (java.lang.System/nanoTime)))
       (println "> unify" (type-string t) (type-string u)))
    (unify t u {}))
  ([^IType t ^IType u bindings]
    (cond
      (and (var-type? t) (var-type? u))
        (if (= (.var t) (.var u)) bindings (add-binding bindings (.var t) u))
      (var-type? t)
        (if (occurs-in (.var t) u)
          (when unify-debug
            (println (to-string "unify " t " " u " " (bindings-string bindings) ": circular")))
          (add-binding bindings (.var t) u))
      (var-type? u)
        (recur u t bindings)
      (and (function-type? t) (function-type? u))
        (let [bindings' (unify (.lhs t) (.lhs u) bindings)]
          (when bindings'
            (recur (substitute bindings' (.rhs t))
                   (substitute bindings' (.rhs u)) bindings')))
      (and (list-type? t) (list-type? u))
        (recur (.member t) (.member u) bindings)
      (= (.type t) (.type u))
        bindings
      :else
        (when unify-debug
          (println (to-string "unify " t " " u " " (bindings-string bindings)
                              ": incompatible types"))))))

(defn unify-independent-types [^IType t ^IType u]
  "Unify types whose variables are in different namespaces.
  (unify-independent-types (make-type '(a b b)) (make-type '(b c c))) => true"
  (unify t (negate-vars u)))
#_(def unify-independent-types (unsynced-memoize unify-independent-types'))

;(def unifiable? (comp some? unify))

(defn unify-test [t u unifiable]
  (let [b (unify t u)
        info {:t t :u u :unifiable unifiable :bindings b}]
    (if (nil? b)
      (assoc info :success (not unifiable))
      (as-> info m
        (assoc m :t-unified (substitute b t))
        (assoc m :u-unified (substitute b u))
        (assoc m :t-unified-norm (normalize-vars (:t-unified m)))
        (assoc m :u-unified-norm (normalize-vars (:u-unified m)))
        (assoc m :success (and unifiable (= (:t-unified-norm m) (:u-unified-norm m))))))))

(defn- run-unify-tests [verbose]
  (doall
    (for [[t-raw u-raw unifiable] [
            ['a :int true]
            [:int 'a true]
            ['(a b) '(:int :int) true]
            ['(a :int) '(:int b) true]
            ['(a :int) '(:int a) true]
            ['(a b) '(:int c d) true]
            ['(a b) '(c :int d) true]
            ['(a b) '(:int b c) false]
            ['(a :int) '(:int b c) false]
            ['((a b) a a) '((c e) e c) true]
            ['((a :int) a a) '((c e) e c) true]
            ['((a b) a a) '((c e) :int c) true]
            ['((a b) a a) '((c e) :int [:int]) false]
            ['((a b) a a) '(((d c) e) e c) false]
            ['((a b) a a) '((c e) e (d e)) false]
            ['((:int [:int] :int) (e e) d h) '((b c) (a b) a c) true]
            ['((d :int) ((d b) e) d b) '((b c) (a b) a c) false]
            ['((d :int) ((d b) e) d b) '((b c) (a b) a c) false]
            ['(a a a) '(d (b c) d) true]
          ]]
      (let [t (make-type-unnormalized t-raw)
            u (make-type-unnormalized u-raw)]
        (when verbose
          (println)
          (println (type-string t) "+++" (type-string u)))
        (let [r (unify-test t u unifiable)]
          (when verbose
            (when-not (:success r) (println "** FAILURE **"))
            (println (map binding-string (sort (:bindings r))))
            (when (:bindings r)
              (println (type-string (:t-unified r)) "+++" (type-string (:u-unified r)))
              (println (type-string (:t-unified-norm r)) "+++" (type-string (:u-unified-norm r)))))
          r)))))
(deftest unify-unit-test
  (let [failures (count (filter (complement :success) (run-unify-tests false)))]
    (when (pos? failures)
      (run-unify-tests true)
      (println "\n" failures "failures"))
    (is (zero? failures))))

(defn returned-type [^IType tf ^IType tx]
  {:pre [(is (function-type? tf))]}
  (let [tp (.lhs tf)
        tx' (negate-vars tx)
        bindings (unify tp tx')]
;    (println (str "returned-type " tf " " tx))
    (when bindings
      (normalize-vars (substitute bindings (.rhs tf))))))

(defn returned-type-test []
  (doseq [[tf-raw tx-raw] [
                           ['((a b) [a] [b]) '(:int :bool)]
                           ['((a b) [a] [b]) '(a b)]
                           ['((a b) [a] [b]) '(a a)]
                           ['((a b) [a] [b]) '([a] [a])]
                       ]
          :let [[tf tx] (map make-type [tf-raw tx-raw])]]
    (println (type-string<> tf) (type-string<> tx) "="
             (type-string<> (returned-type tf tx)))))

;(println)
;(returned-type-test)

#_(comment
(def subtype-debug true)

(defn subtype-bindings
  ([^IType t ^IType u] (subtype-bindings t u {}))
  ([^IType t ^IType u bindings]
    (when subtype-debug (println (to-string "(subtype " t " " u " " bindings ")")))
    (cond
      (and (var-type? t) (var-type? u))
        (if (= (.var t) (.var u)) bindings (add-binding bindings (.var t) u))
      (var-type? t)
        (when subtype-debug
          (println (to-string "subtype failure " t " <=? " u " " (bindings-string bindings)
                              ": bad var")))
      (var-type? u)
        (if (occurs-in (.var u) t)
          (when subtype-debug
            (println (to-string "subtype failure " t " <=? " u " " (bindings-string bindings)
                                ": circular")))
          (add-binding bindings (.var u) t))
      (and (function-type? t) (function-type? u))
        (let [bindings' (subtype-bindings (.lhs u) (.lhs t) bindings)]
          (when bindings'
            (recur (substitute bindings' (.rhs t))
                   (substitute bindings' (.rhs u)) bindings')))
      (and (list-type? t) (list-type? u))
        (recur (.member t) (.member u) bindings)
      (= (.type t) (.type u))
        bindings
      :else
        (when subtype-debug
          (println (to-string "subtype failure " t " <=? " u " " (bindings-string bindings)
                              ": incompatible types"))))))
(def subtype? (comp some? subtype-bindings))

(deftest subtype-test
  (doseq [[t-raw u-raw subtype] [['a :int false]
                                 [:int 'a true]
                                 ['(a a) '(b :int) false]
                                 ['(a b) '(:int :int) false]
                                 ['(a :int) '(:int b) true]
                                 ['(a b) '(:int c d) false]
                                 ['(a a a) '(:bool :int c) false]
                                 ['(a b c) '(d :int e) true]
                                 ['(a a a) '(c :int d) true]
                                 ['((a b) a a) '((c e) e c) true]
                                 ['((a :int) a :int) '((c :int) c :int) true]
                                 ['(([a] :int) [a] :int) '((c :int) c :int) true]
                                 ['((a :int) a :int) '(([c] :int) [c] :int) true]
                                 ['((a :int) a :int) '(([c] :int) [c] d) true]
                                 ['((a :int) a :int) '(c c :int) false]
                                 ['(a a :int) '((c :int) (c :int) :int) true]
                                 ['(a a :int) '((c :int) (:int :int) :int) true]
                                 ['(a a :int) '((c :int) (c d) :int) true]
                                 ['((a b) c) '(d d) false]]]
    (let [[t u] (map make-type-unnormalized [t-raw u-raw])
          _ (when subtype-debug
              (println "subtype" (to-string t " <=? " u " " subtype)))
          bindings (subtype-bindings t u)
          success (= subtype (some? bindings))]
      (when-not success
        (println "**FAILED** (bindings:" (bindings-string bindings) ")"))
      (is success))))
)

#_(run-tests)

(defn perf-test [T]
  (benchmark (fn [] (run-unify-tests false)) 5000 0)
  (benchmark (fn [] (run-unify-tests false)) 5000 0)
  (benchmark (fn [] (run-unify-tests false)) 5000 0))










