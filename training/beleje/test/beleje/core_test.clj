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

(deftest median-test
  (testing "median with input 1 & 2"
    (is (= 10 (median (repeat 20 10))))
    (is (= 3 (median data1)))
    (is (= 5.6 (median data2))))
  (testing "median with input 3"
    (is (= {:iq (/ (+ 120 130) 2)}
           (median [:iq] data3)))
    (is (= {:eq (/ (+ 100 130) 2)}
           (median [:eq] data3)))))

(deftest quartile-test
  (testing "quartile with input 1 & 2"
    (is (= {:q1 10.0 :q2 10.0 :q3 10.0}
           (quartile (repeat 20 10))))
    (is (= {:q1 1.0 :q2 3.0 :q3 6.0}
           (quartile data1)))
    (is (= {:q1 (/ (+ 2.1 3.2) 2.0) :q2 5.6 :q3 (/ (+ 7 8) 2.0)}
           (quartile data2))))
  (testing "quartile with input 3"
    (is (= {:iq {:q1 102.5 :q2 125.0 :q3 165.0}}
           (quartile [:iq] data3)))
    (is (= {:eq {:q1 90.0 :q2 115.0 :q3 147.5}}
           (quartile [:eq] data3)))
    (is (= {:iq {:q1 102.5 :q2 125.0 :q3 165.0}
            :eq {:q1 90.0 :q2 115.0 :q3 147.5}}
           (quartile [:iq :eq] data3)))))

(deftest decile-test
  (testing "decile with list of numbers inputs"
    (is (= {:d1 430.0, :d2 530.0, :d3 595.0, :d4 630.0, :d5 687.5, :d6 745.0, :d7 800.0, :d8 820.0, :d9 902.5000000000001}
           (decile [500 850 925 800 600 750 650 625 800 400 725 550])))
    (is (= {:d1 4.0 :d2 6.0 :d3 8.5 :d4 10.5 :d5 11.5 :d6 13.5 :d7 17.5 :d8 25.0 :d9 32.0}
           (decile [110 301 501 701 991 201 301 101 111 121 341 151])))))