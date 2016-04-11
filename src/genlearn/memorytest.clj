(ns genlearn.memorytest
  (:use
    genlearn.util))

(set! *warn-on-reflection* true)

(def test-type (first *command-line-args*))
(def M (Integer/parseInt (second *command-line-args*)))
(def N (Integer/parseInt (nth *command-line-args* 2)))

(defn usedMem []
  (System/gc)
  (- (.totalMemory (Runtime/getRuntime)) (.freeMemory (Runtime/getRuntime))))

(defrecord Rec2 [mem1 mem2])
(defrecord RecInt2 [mem1 mem2])

(deftype Obj1 [mem])
(deftype Obj2 [mem1 mem2])
(deftype Obj20 [
                mem00 mem01 mem02 mem03 mem04 mem05 mem06 mem07 mem08 mem09
                mem10 mem11 mem12 mem13 mem14 mem15 mem16 mem17 mem18 mem19])
(defn make-obj20 [] (Obj20. (Integer. 0) (Integer. 1) (Integer. 2) (Integer. 3) (Integer. 4) (Integer. 5) (Integer. 6) (Integer. 7) (Integer. 8) (Integer. 9) (Integer. 10) (Integer. 11) (Integer. 12) (Integer. 13) (Integer. 14) (Integer. 15) (Integer. 16) (Integer. 17) (Integer. 18) (Integer. 19)))

(deftype ObjInt [^int mem])
(deftype ObjInt2 [^int mem1 ^int mem2])

(def create-object
  (case test-type
    "vector" #(vec (range M))
    "list" #(doall (range M))
    "array" #(int-array M)
    "tuple" #(apply tuple (range M))
    "integer" #(Integer. 123)
    "record2" #(Rec2. (Integer. 1) (Integer. 2))
    "recordprim2" #(RecInt2. (Integer. 1) (Integer. 2))
    "object1" #(Obj1. (Integer. 1))
    "object2" #(Obj2. (Integer. 1) (Integer. 2))
    "object20" #(make-obj20)
    "objectprim" #(ObjInt. 1)
    "objectprim2" #(ObjInt2. 1 2)))

(defn allocate [^objects array]
  (loop [i 0]
    (when (< i (alength array))
      (aset array i (create-object))
      (recur (inc i)))))

(def array (make-array (type (create-object)) N))

(dotimes [t 3]
  (System/gc)
  (let [array (object-array N)]
    (let [origMem (usedMem)]
      (allocate array)
      (let [allocatedMem (usedMem)]
        (printf "N = %d: %d - %d = %d (%.2f per object)%n"
                (alength array)
                allocatedMem
                origMem
                (- allocatedMem origMem)
                (/ (double (- allocatedMem origMem)) N))))))


      