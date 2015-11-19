(ns teraining.core-test
  (:require [clojure.test :refer :all]
            [teraining.core :refer :all]))

(deftest a-test
  (testing "FUCK MEH!!!!!!!!!!!!!!!!!!!!!!!!"
    (is (= 1 (/ 30 (* 5 3 (- 10 4 (/ 24 6))))))))


(deftest statistics
  (testing "this is what i mean"

    (is (= 58
            (means [1 2 3 3 1 2 443 12])))
    (is (= 499999
           (means (range 1000000))))
    (is (= 5
           (means (range 11))))

    (is (= {:angka 426, :beda 226, :cara 53}
           (#(means % %2) [:angka :beda :cara]
             [{:tampan 0 :angka 300 :pepet 30 :beda 50 :cara 150}
              {:tampan 0 :angka 909 :pepet 30 :beda 40 :cara 10}
              {:tampan 0 :angka 69 :pepet 303 :beda 588 :cara 1}]))))
  
  (testing "this is what i frequencies"))