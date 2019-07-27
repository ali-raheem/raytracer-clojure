(ns raytracer.ray
  (:require [raytracer.vec3 :as vec3]))

(defn point-at-t
  [ray t]
  (let [a (:origin ray)
        b (:direction ray)]
    (vec3/add a (vec3/mul-scalar b t))))

(defn ray
  ([a b] 
   {:origin a,
    :direction b})
  ([ai aj ak bi bj bk] 
   (ray 
    (vec3/make-vec ai aj ak) 
    (vec3/make-vec bi bj bk))))

(defn make-camera-ray [u v]
  (let [lower_left_corner (vec3/make-vec -2 -1 -1)
      horizontal (vec3/make-vec 4 0 0)
      vertical (vec3/make-vec 0 2 0)
      origin (vec3/make-vec 0 0 0)]
    (ray
     origin
     (reduce vec3/add (list 
                  lower_left_corner 
                  (vec3/mul-scalar horizontal u) 
                  (vec3/mul-scalar vertical v))))))
