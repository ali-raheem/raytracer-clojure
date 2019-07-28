(ns raytracer.core
  (:require [raytracer.vec3 :as vec3]
            [raytracer.ray :as ray]
            [raytracer.sphere :as sphere])
  (:gen-class))

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

(defn colour 
  [ray hitelem tmin tmax]
  (let [hitfn (:hit hitelem)
        centre (:centre hitelem)
        radius (:radius hitelem)
        rec (hitfn centre radius ray tmin tmax)]
    (if (some? rec)
      (let [t (:t rec)
            N (:n rec)
            p (:p rec)]
; TODO thread as
        {:t t
         :colour (vec3/mul-scalar 
                  (apply vec3/make-vec 
                         (map inc 
                              (vals (vec3/make-unit N)))) 0.5)})
      (let [dir (vec3/make-unit (:direction ray))
            t (* (+ (:j dir) 1) 0.5)]
        {:t tmax
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

(defn -gen-line 
  [w h y]
  (loop [x 0
         coll '()]
    (if (= x w)
      (reverse coll)
      (do
        (let [hit (detect-hits 
                   (ray/make-camera-ray 
                    (/ x w) (/ y h)) 
                     myhitlist 
                     0.00001
                     10000000)
              new-coll (-coll-filter coll hit)]
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
;       -rgb-to-int ; TODO not needed yet
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
   (clojure.string/join "\n" (map get-rgb (-gen-frame w h)))
   "\n"))

(defn write-image [file img]
  (spit file img))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (write-image "output.ppm" (get-image 800 400)))
