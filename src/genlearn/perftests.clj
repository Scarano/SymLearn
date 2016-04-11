(ns genlearn.perftests)

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)


(def tests {
            :vector-v-array false
            :record-v-type true})


(when (tests :small-fn)
  (time
    (dotimes [i 5000000]
      (let [a [0 0]]
        (-> a
          (assoc 0 i)
          (assoc 1 i)
          (assoc 0 (* (nth a 0) (nth a 1)))
          (assoc 1 (+ (nth a 0) (nth a 1)))
          (assoc 0 (* (nth a 0) (nth a 1)))
          (assoc 1 (+ (nth a 0) (nth a 1)))
          (assoc 0 (* (nth a 0) (nth a 1)))
          (assoc 1 (+ (nth a 0) (nth a 1)))
          hash))))
  (time
    (dotimes [i 5000000]
      (let [a (int-array 2)]
        (aset a 0 i)
        (aset a 1 i)
        (aset a 0 (* (aget a 0) (aget a 1)))
        (aset a 1 (+ (aget a 0) (aget a 1)))
        (aset a 0 (* (aget a 0) (aget a 1)))
        (aset a 1 (+ (aget a 0) (aget a 1)))
        (aset a 0 (* (aget a 0) (aget a 1)))
        (aset a 1 (+ (aget a 0) (aget a 1)))
        (hash a)))))

(defrecord R [a b c])
(defrecord Rp [^long a ^long b ^long c])
(definterface IR2
  (^long a [])
  (^long b [])
  (^long c []))
(defrecord R2 [^long a ^long b ^long c]
  IR2
  (^long a [this] a)
  (^long b [this] b)
  (^long c [this] c))
  
(deftype T [a b c])
(deftype Tp [^long a ^long b ^long c])

(when (tests :record-v-type)
  (println "defrecord-as-map seq reduce")
  (time
    (dotimes [t 5000]
      (let [rs (map #(->R % % %) (range 1000))]
        (as-> 0 acc
          (reduce (fn [v rec] (+ v (:a rec) (:b rec) (:c rec))) acc rs)
          (reduce (fn [v rec] (+ v (:a rec) (:b rec) (:c rec))) acc rs)
          (reduce (fn [v rec] (+ v (:a rec) (:b rec) (:c rec))) acc rs)))))
  (println "defrecord seq reduce")
  (time
    (dotimes [t 5000]
      (let [rs (map #(->R % % %) (range 1000))]
        (as-> 0 acc
          (reduce (fn [v ^R rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)
          (reduce (fn [v ^R rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)
          (reduce (fn [v ^R rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)))))
  (println "defrecord seq loop")
  (time
    (dotimes [t 5000]
      (let [objs (map #(->R % % %) (range 1000))
            f (fn [x]
                (loop [acc x objs' objs]
                  (if (empty? objs')
                    acc
                    (let [obj ^R (first objs')]
                      (recur (+ acc (.a obj) (.b obj) (.c obj)) (rest objs'))))))]
        (-> 0
          f
          f
          f))))
  (println "defrecord array loop")
  (time
    (dotimes [t 5000]
      (let [objs (into-array (map #(->R % % %) (range 1000)))]
;        (dotimes [i 1000] (aset objs i (R. i i i)))
        (let [f (fn [x ^"[Lgenlearn.perftests.R;" objs']
                  (loop [acc x i 0]
                    (if (= i 1000)
                      acc
                      (let [obj ^R (aget objs' i)]
                        (recur (+ acc (.a obj) (.b obj) (.c obj)) (inc i))))))]
          (-> 0
            (f objs)
            (f objs)
            (f objs))))))
  
  (set! *unchecked-math* :warn-on-boxed)
  (println "defrecord-hinted seq reduce")
  (time
    (dotimes [t 5000]
      (let [rs (map #(->Rp % % %) (range 1000))]
        (as-> 0 acc
          (reduce (fn [^long v ^Rp rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)
          (reduce (fn [^long v ^Rp rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)
          (reduce (fn [^long v ^Rp rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)))))
  (println "defrecord-hinted seq reduce (keys, not fields)")
  (time
    (dotimes [t 5000]
      (let [rs (map #(->Rp % % %) (range 1000))]
        (as-> 0 acc
          (reduce (fn [^long v ^Rp rec] (+ v (:a rec) (:b rec) (:c rec))) acc rs)
          (reduce (fn [^long v ^Rp rec] (+ v (:a rec) (:b rec) (:c rec))) acc rs)
          (reduce (fn [^long v ^Rp rec] (+ v (:a rec) (:b rec) (:c rec))) acc rs)))))
  
  
  (println "defrecord-interfaced seq reduce")
  (time
    (dotimes [t 5000]
      (let [rs (map #(->R2 % % %) (range 1000))]
        (as-> 0 acc
          (reduce (fn [^long v ^R2 rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)
          (reduce (fn [^long v ^R2 rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)
          (reduce (fn [^long v ^R2 rec] (+ v (.a rec) (.b rec) (.c rec))) acc rs)))))
  
  (set! *unchecked-math* true)
  (println "deftype seq reduce")
  (time
    (dotimes [t 5000]
      (let [objs (map #(->T % % %) (range 1000))]
        (as-> 0 acc
          (reduce (fn [v ^T obj] (+ v (.a obj) (.b obj) (.c obj))) acc objs)
          (reduce (fn [v ^T obj] (+ v (.a obj) (.b obj) (.c obj))) acc objs)
          (reduce (fn [v ^T obj] (+ v (.a obj) (.b obj) (.c obj))) acc objs)))))
  (set! *unchecked-math* :warn-on-boxed)
  (println "deftype-hinted seq reduce")
  (time
    (dotimes [t 5000]
      (let [objs (map #(->Tp % % %) (range 1000))]
        (as-> 0 acc
          (reduce (fn [^long v ^Tp obj] (+ v (.a obj) (.b obj) (.c obj))) acc objs)
          (reduce (fn [^long v ^Tp obj] (+ v (.a obj) (.b obj) (.c obj))) acc objs)
          (reduce (fn [^long v ^Tp obj] (+ v (.a obj) (.b obj) (.c obj))) acc objs)))))
  (println "deftype-hinted seq loop")
  (time
    (dotimes [t 5000]
      (let [objs (map #(->Tp % % %) (range 1000))
            f (fn [x]
                (loop [acc x objs' objs]
                  (if (empty? objs')
                    acc
                    (let [obj (first objs')]
                      ; fn is introduced in following line only to provide type hint
                      (recur (+ x (.a ^Tp obj) (.b ^Tp obj) (.c ^Tp obj)) (rest objs'))))))]
        (-> 0
          f
          f
          f)))))

