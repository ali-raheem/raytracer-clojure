(ns raytracer.core
  (:gen-class))

(defn make-vec 
  [i j k] 
  (zipmap '(:i :j :k) [i j k]))

(defn mul-scalar [v k]
  (->> v
       vals
       (map #(* k %))
       (apply make-vec)))

(defn add-scalar [v k]
  (->> v
       vals
       (map inc)
       (apply make-vec)))

(defn sqr [x] (* x x))

(defn sqr-length [v]
  (->> v
       vals
       (map #(sqr %))
       (reduce +)))

(defn magnitude [v]
  (Math/sqrt (sqr-length v)))

(defn make-unit [v]
  (mul-scalar v (/ 1 (magnitude v))))

(defn -rgb-to-int [C]
  (-> C
      make-unit
      (add-scalar 1)
      (mul-scalar 0.5)))

(defn map-to-255
  [x]
  (int (* 255.99 x)))

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


(defn -gen-line 
  [w h y]
  (loop [x 0
         coll '()]
    (if (= x w)
      (reverse coll)
      (do
        (let [new-coll (conj coll (make-vec (/ x w) (/ y h) 0.2))]
          (recur (inc x) new-coll))))))

(defn -gen-frame 
  [w h]
  (loop [y (dec h)
         coll '()]
    (if (= -1 y)
      coll
      (let [new-coll (concat coll (-gen-line w h y))]
        (recur (dec y) new-coll)))))

(defn get-ppm-header [w h] (str "P3\n" w " " h "\n255\n" ))
(defn get-rgb [cols]
  (->> cols
       -rgb-to-int
       vals
       (map map-to-255)
       (interpose " ")
       (apply str)))
(defn get-colours [cols] (map str (get-rgb cols)))

(defn get-image [w h]
  (str (get-ppm-header w h) (clojure.string/join "\n" (map get-rgb (-gen-frame w h)))))

(defn write-image [file img]
  (spit file img))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (write-image "test.ppm" (get-image 200 100)))
