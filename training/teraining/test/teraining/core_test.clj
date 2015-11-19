(ns teraining.core-test
  (:require [clojure.test :refer :all]
            [teraining.core :refer :all]))

(deftest a-test
  (testing "FUCK MEH!!!!!!!!!!!!!!!!!!!!!!!!"
    (is (= 1 (/ 30 (* 5 3 (- 10 4 (/ 24 6))))))))


(deftest statistics
  (testing "if you know what i mean"

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

  (testing "if you know what i variance"
    (is (= 2.5
           (variance [1 2])))
    (is (= "Jangan goblok"
           (variance [])))
    (is (= 3283.5
           (variance (range 100))))
    (is (= {:pancoran 777708.3, :dalam 2907.6667, :bulan 5124.3335}
           (#(variance % %2) [:pancoran :dalam :bulan][{:patung 150 :pancoran 150 :dalam 91 :pejam  150 :bulan 123}
                                                       {:patung 50 :pancoran 15 :dalam 9 :pejam  15 :bulan 12}
                                                       {:patung 1500 :pancoran 1520 :dalam 19 :pejam  50 :bulan 10}] )))))