(ns beleje.core-test
  (:require [clojure.test :refer :all]
            [beleje.core :refer :all]))

(def data1
  [1 0 10 9 8 7 6 5 4 3 3 3 3 2 2 2 1 1 0])

(def data2
  [1.0 2.1 3.2 4.3 5.6 7 7 8 8])


(def data3
  [{:name "Jon Snow" :iq 100 :eq 160 :status "deceased"}
   {:name "Tyrion Lannister" :iq 150 :eq 130 :status "missing"}
   {:name "Daenarys Targaryen" :iq 130 :eq 100 :status "alive"}
   {:name "Sansa Stark" :iq 90 :eq 90 :status "missing"}
   {:name "Petyr Baelish" :iq 180 :eq 70 :status "alive"}
   {:name "Varys" :iq 170 :eq 150 :status "missing"}
   {:name "Ned Stark" :iq 120 :eq 140 :status "deceased"}
   {:name "Cersei Lannister" :iq 110 :eq 90 :status "alive"}])

(deftest means-test
  (testing "means with input1 & 2"
    (is (= 10 (means (repeat 20 10))))
    (is (= (/ (apply + data1) (count data1))
           (means data1)))
    (is (= (/ (apply + data2) (count data2))
           (means data2))))
  (testing "means with input 3"
    (is (= {:iq (/ (+ 100 150 130 90 180 170 120 110) 8)}
           (means [:iq] data3)))
    (is (= {:iq (/ (+ 100 150 130 90 180 170 120 110) 8)
            :eq (/ (+ 160 130 100 90 70 150 140 90) 8)}
           (means [:iq :eq] data3)))))

(deftest mode-test
  (testing "mode with input1 & 2"
    (is (= [10] (mode (repeat 20 10))))
    (is (= [3] (mode data1)))
    (is (= [7 8] (mode data2))))
  (testing "mode with input 3"
    (is (= {:iq [100 150 130 90 180 170 120 110]}
           (mode [:iq] data3)))
    (is (= {:eq [90]} (mode [:eq] data3)))
    (is (= {:status ["missing" "alive"]} (mode [:status] data3)))
    (is (= {:iq [100 150 130 90 180 170 120 110]
            :eq [90]
            :status ["missing" "alive"]}
           (mode [:iq :eq :status] data3)))))

(deftest variance-test
  (testing "variance with input 1 & 2"
    (is (= 0 (variance (repeat 20 10))))
    (is (= 220 (variance [-1 1 10 -10 3 -3])))
    (is (= 3118/19 (variance data1)))
    (is (= 54.34 (variance data2))))
  (testing "variance with input 3"
    (is (= {:iq 14975/2} (variance [:iq] data3)))
    (is (= {:eq 15175/2} (variance [:eq] data3)))
    (is (= {:iq 14975/2 :eq 15175/2} (variance [:iq :eq] data3)))))

(deftest std-test
  (testing "std with input1 & 2"
    (is (= 0.0 (std (repeat 20 10))))
    (is (= 14.832396974191326 (std [-1 1 10 -10 3 -3])))
    (is (= 12.81035765144341 (std data1)))
    (is (= 7.371566997592846 (std data2))))
  (testing "std with input 3"
    (is (= {:iq 86.53034149938391} (std [:iq] data3)))
    (is (= {:eq 87.10625695092173} (std [:eq] data3)))
    (is (= {:iq 86.53034149938391 :eq 87.10625695092173} (std [:iq :eq] data3)))))

(deftest freq-test
  (testing "freq with input1 & 2"
    (is (= {10 20} (freq (repeat 20 10))))
    (is (= {10 5 0 2 3 1} (freq [10 3 0 0 10 10 10 10])))
    (is (= (frequencies data1) (freq data1)))
    (is (= (frequencies data2) (freq data2))))
  (testing "freq with input 3"
    (is (= {:iq (frequencies (list 100 150 130 90 180 170 120 110))}
           (freq [:iq] data3)))
    (is (= {:eq (frequencies (list 160 130 100 90 70 150 140 90))}
           (freq [:eq] data3)))
    (is (= {:status (frequencies (list "deceased" "missing" "alive" "missing" "alive" "missing" "deceased" "alive"))}
           (freq [:status] data3)))
    (is (= {:iq (frequencies (list 100 150 130 90 180 170 120 110))
            :eq (frequencies (list 160 130 100 90 70 150 140 90))
            :status (frequencies (list "deceased" "missing" "alive" "missing" "alive" "missing" "deceased" "alive"))}
           (freq [:iq :eq :status] data3)))))

(deftest freq-by-test
  (testing "freq-by with input 1 & 2"
    (is (= {:0-4 0 :5-7 0 :8-10 20}
           (freq-by nilai (repeat 20 10))))
    (is (= {:0-4 13 :5-7 3 :8-10 3}
           (freq-by nilai data1)))
    (is (= {:0-4 4 :5-7 3 :8-10 2}
           (freq-by nilai data2))))
  (testing "freq-by with input 3"
    (is (= {:iq {:<=100 2 :101-130 3 :>131 3}}
           (freq-by eq-iq-group [:iq] data3)))
    (is (= {:eq {:<=100 4 :101-130 1 :>131 3}}
           (freq-by eq-iq-group [:eq] data3)))
    (is (= {:status {:survivor 6 :stupid 2}}
           (freq-by survivor-count [:status] data3)))
    (is (= {:iq {:<=100 2 :101-130 3 :>131 3}
            :eq {:<=100 4 :101-130 1 :>131 3}}
           (freq-by eq-iq-group [:iq :eq] data3)))))