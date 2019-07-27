(ns raytracer.core-test
  (:require [clojure.test :refer :all]
            [raytracer.core :refer :all]))

(deftest make-vec-test
  (testing "make-vec"
    (is (= (make-vec 1 2 3) {:i 1 :j 2 :k 3}))))
