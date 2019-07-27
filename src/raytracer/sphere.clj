(ns raytracer.sphere
  (:require [raytracer.vec3 :as vec3]
            [raytracer.ray :as ray]))

(defn hit
  [centre radius ray]
  (let [oc (vec3/sub (:origin ray) centre)
        a (vec3/dot (:direction ray) (:direction ray))
        b (* (vec3/dot oc (:direction ray)) 2)
        c (- (vec3/dot oc oc) (* radius radius))
        discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      -1
      (/ (- (+ b (Math/sqrt discriminant))) (* 2 a)))))

