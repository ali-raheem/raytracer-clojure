(ns raytracer.core
  (:require [raytracer.vec3 :as vec3]
            [raytracer.ray :as ray]
            [raytracer.sphere :as sphere])
  (:gen-class))

(def bounce-depth 100)

(defn hitable
  [centre radius hit]
  {:centre centre
   :radius radius
   :hit hit})

(def myhitlist (list
                (hitable  (vec3/make-vec -1 0 -1) 0.5 sphere/hit)
                (hitable  (vec3/make-vec 0 0 -1) 0.5 sphere/hit)
                (hitable  (vec3/make-vec 0 -100.5 -1) 100 sphere/hit)
                (hitable  (vec3/make-vec 1 0 -1) 0.5 sphere/hit)
                ))


(defn -rgb-to-int [C]
  (-> C
      vec3/make-unit
      (vec3/add-scalar 1)
      (vec3/mul-scalar 0.5)))

(defn map-to-255
  [x]
  {:pre [(number? x)]
   :post [(> 256 %), (<= 0 %)]}
  (int (* 255.99 x)))

(defn -random-vec []
  (vec3/make-vec (rand) (rand) (rand)))

(defn -random-point-sphere-vec []
  (vec3/sub (vec3/mul-scalar  (-random-vec) 2) (vec3/make-vec 1 1 1)))

(defn random-point-sphere []
  (loop [p (-random-point-sphere-vec)]
    (if (>= 1 (vec3/sqr-length p))
      p
      (recur (-random-point-sphere-vec)))))

(declare detect-hits)

(defn colour 
  [ray hitelem tmin tmax]
  (let [hitfn (:hit hitelem)
        centre (:centre hitelem)
        radius (:radius hitelem)
        rec (hitfn centre radius ray tmin tmax)]
    (if (some? rec)
      (let [t (:t rec)
            n (:n rec)
            p (:p rec)]
; TODO thread as
        (let [target (vec3/add p (vec3/add n (random-point-sphere)))]
          {:t t
           :p p
           :n n
           :colour (vec3/mul-scalar 
                    (detect-hits 
                     (ray/ray p target) 
                     myhitlist 0.0001 100000)
                    0.5)}))
      (let [dir (vec3/make-unit (:direction ray))
            t (* (+ (:j dir) 1) 0.5)
            p (ray/point-at-t ray t)
            n (vec3/make-vec 0 0 -1)]
        {:t tmax
         :p p
         :n n
         :colour (vec3/add 
                  (vec3/mul-scalar 
                   (vec3/make-vec 0.5 0.7 1) t)
                  (vec3/mul-scalar 
                   (vec3/make-vec 1 1 1) (- 1 t)))}))))

(defn -filter-rec
  [& recs]
  (->> recs
       (filter some?)
       (filter #(some? (:t %)))
       (sort-by :t)
       first))

(defn -closest-filter
  [rec t]
  (if (number? (:t rec))
    (:t rec)
    t))

(defn detect-hits 
  [ray world tmin tmax]
;  (println "detect-hits" ray world tmin tmax)
  (loop [world world
         closest tmax
         rec nil]
    (if (empty? world)
      (:colour rec)
      (let [obj (first world)
            hitrec (colour ray obj tmin closest)
            -rec (-filter-rec hitrec rec)
            -closest (-closest-filter -rec tmax)]
        (recur (rest world) tmax -rec)))))

(defn -coll-filter
  [coll hit]
  (if (some? hit)
    (conj coll hit)
    coll))

(defn super-sample
  [x y w h bounce-depth]
  (loop [ns 0
         col (vec3/make-vec 0 0 0)]
    (if (= bounce-depth ns)
      (vec3/mul-scalar col (/ 1 ns))
      (let [nx (+ (rand) x)
            ny (+ (rand) y)
            new-col (vec3/add col
                              (detect-hits 
                               (ray/make-camera-ray 
                                (/ nx w) (/ ny h)) 
                               myhitlist 
                               0.00001
                               10000000))]
        (recur (inc ns) new-col)))))

(defn -gen-line 
  [w h y bd]
  (loop [x 0
         coll '()]
    (if (= x w)
      (reverse coll)
      (do
        (let [hit (super-sample x y w h bd)
              new-coll (-coll-filter coll hit)]
          (recur (inc x) new-coll))))))

(defn -gen-frame 
  [w h bd]
  (loop [y (dec h)
         coll '()]
    (if (= -1 y)
      coll
      (let [new-coll (concat coll (-gen-line w h y bd))]
        (recur (dec y) new-coll)))))

(defn get-ppm-header 
  [w h] 
  (str "P3\n" w " " h "\n255\n" ))

(defn get-rgb [cols]
  (->> cols
;       -rgb-to-int ; TODO not needed yet
       vals
       (map #(Math/sqrt %))
       (map map-to-255)
       (interpose " ")
       (apply str)))

(defn get-colours 
  [cols] 
  (map str (get-rgb cols)))

(defn get-image 
  [w h bd]
  (str 
   (get-ppm-header w h) 
   (clojure.string/join "\n" (map get-rgb (-gen-frame w h bd)))
   "\n"))

(defn write-image [file img]
  (spit file img))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (write-image "output.ppm" (get-image 200 100 100)))
