(ns teraining.core-test
  (:require [clojure.test :refer :all]
            [teraining.core :refer :all]))


(deftest a-test
  (testing "FUCK MEH!!!!!!!!!!!!!!!!!!!!!!!!"
    (is (= 1 (/ 30 (* 5 3 (- 10 4 (/ 24 6))))))))

(def coll [{:patung 150 :pancoran 150 :dalam 91 :pejam  150 :bulan 123}
           {:patung 50 :pancoran 15 :dalam 9 :pejam  15 :bulan 12}
           {:patung 1500 :pancoran 1520 :dalam 19 :pejam  50 :bulan 10}])

(def yg-pgn-diambil [:pancoran :dalam :bulan] )

(deftest statistics
  (testing "if you know what i mean"

    (is (= 58
            (means [1 2 3 3 1 2 443 12])))
    (is (= 499999
           (means (range 1000000))))
    (is (= 5
           (means (range 11))))

    (is (= {:angka 426, :beda 226, :cara 53}
            (means [:angka :beda :cara] [{:tampan 0 :angka 300 :pepet 30 :beda 50 :cara 150}
                                         {:tampan 0 :angka 909 :pepet 30 :beda 40 :cara 10}
                                         {:tampan 0 :angka 69 :pepet 303 :beda 588 :cara 1}]))))

  (testing "he is so variance"
    (is (= 2.5
           (variance [1 2])))
    (is (= "ばか"
           (variance [])))
    (is (= 3283.5
           (variance (range 100)))))

  (testing "a standard man knows that deviation knows nothing"
    (is (= 288.38631497813253
           (standard-deviation (range 1 1000))))
    (is (= {:pancoran 679.8818524812478, :dalam 36.53309002352069, :bulan 52.80467151051442}
           (standard-deviation yg-pgn-diambil coll))))

  (testing "Bahe-mode"
    (is (= [1 7]
           (mode [1 1 1 2 2 3 3 2 2 1 1 1 1])))
    (is (= {:pancoran [1520 1], :dalam [19 1], :bulan [10 1]}
           (mode yg-pgn-diambil
                  coll))))

  (testing "quartilians"
    (is (= [21.0 23 26.5]
           (quartiles [19 21 21 22 23 24 25 28 30])))))

