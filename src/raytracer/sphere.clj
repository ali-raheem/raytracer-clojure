(ns raytracer.sphere
  (:require [raytracer.vec3 :as vec3]
            [raytracer.ray :as ray]))

(defn tmin-tmax [tmin tmax t]
  (and (> t tmin) (< t tmax)))

(defn -hit-calc-roots
  [tmin tmax b d a]
  (let [t  (/ (- (+ b (Math/sqrt d))) a)]
    (if (tmin-tmax tmin tmax t)
      t
      (let [t  (/ (- (- b (Math/sqrt d))) a)]
        (if (tmin-tmax tmin tmax t)
          t
          nil)
))))

(defn hit
  [centre radius ray tmin tmax]
  (let [oc (vec3/sub (:origin ray) centre)
        a (vec3/dot (:direction ray) (:direction ray))
        b (vec3/dot oc (:direction ray)) 
        c (- (vec3/dot oc oc) (* radius radius))
        discriminant (- (* b b) (* a c))]
    (if (neg? discriminant)
      nil
      (if-let [t (-hit-calc-roots tmin tmax b discriminant a)]
        (let [p (ray/point-at-t ray t)
              n (vec3/sub p (vec3/make-vec 0 0 -1))]
          {:t t :p p :n n})
        nil))))
