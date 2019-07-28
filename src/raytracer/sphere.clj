(ns raytracer.sphere
  (:require [raytracer.vec3 :as vec3]
            [raytracer.ray :as ray]))

(defn sphere
  [centre radius]
  {:centre centre
   :radius radius})

(defn tmin-tmax
  [tmin tmax t]
  {:pre [(number? tmin), (number? tmax), (number? t)]}
  (and (> t tmin) (< t tmax)))

(defn -hit-calc-roots
  [tmin tmax b d a]
  {:pre [(number? tmin)
         (number? tmax)
         (number? d)
         (number? a)
         (number? b)]}
  (if-let [t  (/ (- (+ b (Math/sqrt d))) a)]
    (if (tmin-tmax tmin tmax t)
      t
      (if-let [t  (/ (- (- b (Math/sqrt d))) a)]
        (if (tmin-tmax tmin tmax t)
          t
          nil)))))

(defn hit
  [centre radius ray tmin tmax]
;  (println centre radius)
  (let [oc (vec3/sub (:origin ray) centre)
        a (vec3/dot (:direction ray) (:direction ray))
        b (vec3/dot oc (:direction ray)) 
        c (- (vec3/dot oc oc) (* radius radius))
        discriminant (- (* b b) (* a c))]
    (if (neg? discriminant)
      nil
      (if-let [t (-hit-calc-roots tmin tmax b discriminant a)]
        (let [p (ray/point-at-t ray t)
              n (vec3/sub p centre)]
          {:t t :p p :n n})
        nil))))
