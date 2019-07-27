(ns raytracer.core
  (:require [raytracer.vec3 :as vec3]
            [raytracer.ray :as ray])
  (:gen-class))

(defn -rgb-to-int [C]
  (-> C
      vec3/make-unit
      (vec3/add-scalar 1)
      (vec3/mul-scalar 0.5)))

(defn map-to-255
  [x]
  (int (* 255.99 x)))

(defn hit-sphere
  [centre radius ray]
  (let [oc (vec3/sub (:origin ray) centre)
        a (vec3/dot (:direction ray) (:direction ray))
        b (* (vec3/dot oc (:direction ray)) 2)
        c (- (vec3/dot oc oc) (* radius radius))
        discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      -1
      (/ (- (+ b (Math/sqrt discriminant))) (* 2 a)))))

(defn colour 
  [ray]
  (let [t (hit-sphere (vec3/make-vec 0 0 -1) 0.5 ray)]
    (if (pos? t)
      (let [N (vec3/sub (ray/point-at-t ray t) (vec3/make-vec 0 0 -1))]
; TODO thread as
        (vec3/mul-scalar 
         (apply vec3/make-vec 
                (map inc 
                     (vals N))) 0.5))
      (let [dir (vec3/make-unit (:direction ray))
            t (* (+ (:j dir) 1) 0.5)]
        (vec3/add 
         (vec3/mul-scalar (vec3/make-vec 0.5 0.7 1) t)
         (vec3/mul-scalar (vec3/make-vec 1 1 1) (- 1 t)))))))


(defn -gen-line 
  [w h y]
  (loop [x 0
         coll '()]
    (if (= x w)
      (reverse coll)
      (do
        (let [new-coll (conj coll (colour (ray/make-camera-ray (/ x w) (/ y h))))]
          (recur (inc x) new-coll))))))

(defn -gen-frame 
  [w h]
  (loop [y (dec h)
         coll '()]
    (if (= -1 y)
      coll
      (let [new-coll (concat coll (-gen-line w h y))]
        (recur (dec y) new-coll)))))

(defn get-ppm-header 
  [w h] 
  (str "P3\n" w " " h "\n255\n" ))

(defn get-rgb [cols]
  (->> cols
    ;   -rgb-to-int ;; TODO this is not required til later in book
       vals
       (map map-to-255)
       (interpose " ")
       (apply str)))

(defn get-colours 
  [cols] 
  (map str (get-rgb cols)))

(defn get-image 
  [w h]
  (str 
   (get-ppm-header w h) 
   (clojure.string/join "\n" (map get-rgb (-gen-frame w h)))))

(defn write-image [file img]
  (spit file img))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (write-image "output.ppm" (get-image 800 400)))
