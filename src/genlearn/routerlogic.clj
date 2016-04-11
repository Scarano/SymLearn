(ns genlearn.routerlogic
  (:use [genlearn.util])
  (:use [clojure.stacktrace]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:dynamic *debug* false)

(def primitives {
                 '_0 0
                 '_1 1
                 '+ #(partial + %)
                 '* #(partial * %)
})

(defn preprocess [quoted-expr]
  (if (seq? quoted-expr)
    (let [[comb lhs rhs] quoted-expr]
      (list (str comb) (preprocess lhs) (preprocess rhs)))
    quoted-expr))

(defn filter-args [desired comb args]
  (mapcat (fn [c a] (if (desired c) (list a) '())) comb args))
(def filter-args-left (partial filter-args #{\S \C}))
(def filter-args-right (partial filter-args #{\S \B}))

(defn r-eval [r-exp args env]
  (when *debug*
    (println (list "r-eval" r-exp args)))
  (cond
    (= r-exp 'I)
      (first args)
    (not (seq? r-exp))
      (env r-exp)
    :else
      (let [[comb raw-lhs raw-rhs] r-exp
            arity (count comb)
            num-args (count args)
            f (fn [& a]
                (let [rhs-args (filter-args-right comb a)
                      rhs (r-eval raw-rhs rhs-args env)
                      lhs-args (filter-args-left comb a)
                      lhs (r-eval raw-lhs lhs-args env)]
                  (when *debug*
                    (println "applying:" (cons raw-lhs lhs-args) (cons raw-rhs rhs-args))
                    (println "applying:" lhs rhs))
                  (lhs rhs)))]
        (when *debug*
          (println "args:" comb args (filter-args-left comb args) (filter-args-right comb args)))
        (cond
          (< num-args arity)
            (apply partial (cons f args))
          (= num-args arity)
            (apply f args)
          :else ; num-args > arity + 1
            (list 'error (str "num-args (" num-args ") > arity (" arity ")") r-exp args)))))

(def example-inc
  (preprocess
    '(C (B + I) _1)))
(def example-inc-with-lambda
  (preprocess
    '(C (BC I I) ("" + _1))))
(def example-inc-with-lambda2
  (preprocess
    '(C (BC I I) (C (B + I) _1))))
(def example-*f11x
  (preprocess
    '(CC (CBC (BC I (C (C I _1) _1)) I) (BC (B * I) I))))
;    '(CC (CBC (BC I (C (C I _1) _1)) I) *)))
(def example-x2+y
  (preprocess
    '(CB (B + (S (B * I) I)) I)))

(doseq [[example args] [
                        [example-inc '(10)]
                        [example-inc-with-lambda '()]
                        [example-inc-with-lambda '(10)]
                        [example-inc-with-lambda2 '(10)]
                        [example-*f11x (list (primitives '+) 10)]
                        [example-x2+y '(10 1000)]
        ]]
  (println)
  (println "Applying" example "to" args)
  (println (r-eval example args primitives)))

(when true
  (binding [*debug* false]
    (println)
    (doseq [[example args] [
	                           [example-inc '(10)]
	                           [example-inc-with-lambda '()]
	                           [example-inc-with-lambda '(10)]
	                           [example-inc-with-lambda2 '(10)]
                             [example-*f11x (list (primitives '+) 10)]
                             [example-x2+y '(10 1000)]
	        ]]
      (println example args)
      (time
        (dotimes [t 100000]
          (r-eval example args primitives)))))
)

















