(ns raytracer.vec3)

(defn sqr-length [v]
  (->> v
       vals
       (map #(* % %))
       (reduce +)))

(defn make-vec 
  [i j k] 
  (zipmap '(:i :j :k) [i j k]))

(defn mul-scalar [v k]
  (->> v
       vals
       (map #(* k %))
       (apply make-vec)))


(defn -add-sub
  [op]
  (fn
    [a b]
    (let [ac (vals a)
          bc (vals b)]
      (apply make-vec (map op ac bc)))))


(def add (-add-sub +))
(def sub (-add-sub -))

(defn add-scalar [v k]
  (->> v
       vals
       (map #(+ k %))
       (apply make-vec)))


(defn magnitude [v]
  (Math/sqrt (sqr-length v)))


(defn make-unit [v]
  (mul-scalar v (/ 1 (magnitude v))))


(defn cross
  [a b]
  (let [ai (:i a)
        bi (:i b)
        aj (:j a)
        bj (:j b)
        ak (:k a)
        bk (:k b)]
    {:i (- (* aj bk) (* ak bj))
     :j (- (* ak bi) (* ai bk))
     :k (- (* ai bj) (* aj bi))}))

(defn dot
  [a b]
  (let [ai (:i a)
        bi (:i b)
        aj (:j a)
        bj (:j b)
        ak (:k a)
        bk (:k b)]
    (reduce + 
            (list 
             (* ai bi)
             (* aj bj)
             (* ak bk)))))
