(ns genlearn.curry)

(defn curry2 [f]
  (fn [a]
    (fn [b] (f a b))))
(defn curry3 [f]
  (fn [a]
    (fn [b]
      (fn [c] (f a b c)))))
(defn curry4 [f]
  (fn [a]
    (fn [b]
      (fn [c]
        (fn [d] (f a b c d))))))
(defn curry5 [f]
  (fn [a]
    (fn [b]
      (fn [c]
        (fn [d]
          (fn [e] (f a b c d e)))))))

; the rest of this is stolen from https://gist.github.com/sunilnandihalli/745654

(defmacro def-curry-fn [name args & body]
  {:pre [(not-any? #{'&} args)]}
  (if (empty? args)
    `(defn ~name ~args ~@body)
    (let [rec-funcs (reduce (fn [l v]
                              `(letfn [(helper#
                                         ([] helper#)
                                         ([x#] (let [~v x#] ~l))
                                         ([x# & rest#] (let [~v x#]
                                                         (apply (helper# x#) rest#))))]
                                 helper#))
                            `(do ~@body) (reverse args))]
      `(defn ~name [& args#]
         (let [helper# ~rec-funcs]
           (apply helper# args#))))))

;#_(def-curry-fn h [x y z]
;    (+ x y z))
;#_((h 1 2) 3) => 6
;#_((((h) 1) 2) 3) => 6
;
;#_(def-curry-fn ggg [x y]
;    [x y])
;#_(ggg)
;#_((ggg :hello) :hgg) => [:hello :hgg]
;#_(ggg :h :hgg) => [:h :hgg]
;
;
;#_(def-curry-fn f [{:keys [x y] :as w} [a b & d :as e]]
;    [x y w a b d e])
;
;#_((f {:x 10 :y 20 :z 30}) [1 :b 'c :e 3.0]) => [10 20 {:x 10 :y 20 :z 30} 1 :b (c :e 3.0) [1 :b c :e 3.0]]
