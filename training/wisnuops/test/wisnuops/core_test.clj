(ns wisnuops.core
  (:require
    [clojure.test :refer :all]
    [clojure.repl :refer :all]))

(def datadiri
  [{:nama "wisnu" :tinggi 172 :berat 65 :fourclojure 88}
   {:nama "peter" :tinggi 174 :berat 75 :fourclojure 132}
   {:nama "donna" :tinggi 171 :berat 52 :fourclojure 108}
   {:nama "davin" :tinggi 171 :berat 50 :fourclojure 98}])

(deftest testing-means
  (testing "Testing Numeric Data"
    (is (= (means [1 2 3 4 5 6 7 8 9 10]) 11/2) "rata-ratanya harus 11/2")
    (is (= (means [3 4 5 6]) 9/2) "fail untuk data vector")
    (is (= (means '(3 4 5 6)) 9/2) "fail untuk data list")
    (is (= (means #{3 4 5 6}) 9/2) "fail untuk data set"))
  (testing "Testing Multi Dimentional Data"
    (is (= (means :tinggi datadiri) (means [172 174 171 171])) "fail nih untuk :tinggi")
    (is (= (means :berat datadiri) (means [65 75 52 50])) "fail nih untuk :berat")
    (is (= (means :fourclojure datadiri) (means [88 132 108 98])) "fail nih untuk :fourclojure")
    (is (= (means :kalogaada datadiri) :key-not-exist) "fail nih untuk :kalogaada")))

(deftest testing-freq
  (testing "Testing Numeric Data"
    (is (= (freq [1 2 3 2 3 1 2 3]) {1 2, 2 3, 3 3}) "fail nih...")
    (is (= (freq [:a :b :c :d :d :a :a :b]) {:a 3, :b 2, :c 1, :d 2}) "fail nih...")
    (is (= (freq [:a :b :c :d :d :a :a :b]) {:a 3, :d 2, :b 2, :c 1}) "fail nih..."))
  (testing "Testing Multi Dimentional Data"
    (is (= (freq :nama datadiri) (freq ["wisnu" "peter" "donna" "davin"])) "fail di :nama")
    (is (= (freq :tinggi datadiri) (freq [172 174 171 171])) "fail di :tinggi")
    (is (= (freq :berat datadiri) (freq [65 75 52 50])) "fail di :berat")
    (is (= (freq :fourclojure datadiri) (freq [88 132 108 98])) "fail di :fourclojure")
    (is (= (freq :kalogaada datadiri) :key-not-exist) "fail di :kalogaada")))

(deftest testing-modes
  (testing "Testing Numeric Data"
    (is (= (modes [1 2 3 4 2 3 4 3 2 3 4 5 6 4 3 5 6 7 7 4 3 3 4 4]) [3 4]) "fail nih...")
    (is (= (modes [:a :b :c :d :d :a :a :b]) [:a]) "fail nih...")
    (is (= (modes [:a :b :c :d :d :a :a :b]) [:a]) "fail nih..."))
  (testing "Testing Multi Dimentional Data"
    (is (= (modes :nama datadiri) (modes ["wisnu" "peter" "donna" "davin"])) "fail di :nama")
    (is (= (modes :tinggi datadiri) (modes [172 174 171 171])) "fail di :tinggi")
    (is (= (modes :berat datadiri) (modes [65 75 52 50])) "fail di :berat")
    (is (= (modes :fourclojure datadiri) (modes [88 132 108 98])) "fail di :fourclojure")
    (is (= (modes :kalogaada datadiri) :key-not-exist) "fail di :kalogaada")))

(deftest testing-variance
  (testing "Testing Numeric Data"
    (is (= (variance [600 470 170 430 300]) 21704) "Fail ketika dites dengan contoh data dari internet")
    (is (= (variance [1 2 3 4 5 6]) 35/12) "fail nih..."))
  (testing "Testing Multi Dimentional Data"    
    (is (= (variance :tinggi datadiri) (variance [172 174 171 171])) "fail di :tinggi")
    (is (= (variance :berat datadiri) (variance [65 75 52 50])) "fail di :berat")
    (is (= (variance :fourclojure datadiri) (variance [88 132 108 98])) "fail di :fourclojure")
    (is (= (variance :kalogaada datadiri) :key-not-exist) "fail di :kalogaada")))

(deftest testing-std-dev
  (testing "Testing Numeric Data"
    (is (= (std-dev [600 470 170 430 300]) (Math/sqrt 21704)) "Fail ketika dites dengan contoh data dari internet")
    (is (= (std-dev [1 2 3 4 5 6]) (Math/sqrt 35/12)) "fail nih..."))
  (testing "Testing Multi Dimentional Data"    
    (is (= (std-dev :tinggi datadiri) (Math/sqrt (variance [172 174 171 171]))) "fail di :tinggi")
    (is (= (std-dev :berat datadiri) (Math/sqrt (variance [65 75 52 50]))) "fail di :berat")
    (is (= (std-dev :fourclojure datadiri) (Math/sqrt (variance [88 132 108 98]))) "fail di :fourclojure")
    (is (= (std-dev :kalogaada datadiri) :key-not-exist) "fail di :kalogaada")))

(deftest testing-freq-by
  (testing "Testing Numeric Data"
    (is (= (freq-by identity [1 2 3 2 3 1 2 3]) {1 2, 2 3, 3 3}) "fail nih...")
    (is (= (freq-by odd? [1 2 3 2 3 1 2 3]) {true 5, false 3}) "fail nih kalau fungsinya odd?")
    (is (= (freq-by identity [:a :b :c :d :d :a :a :b]) {:a 3, :b 2, :c 1, :d 2}) "fail nih...")
    (is (= (freq-by identity [:a :b :c :d :d :a :a :b]) {:a 3, :d 2, :b 2, :c 1}) "fail nih..."))
  (testing "Testing Multi Dimentional Data"
    (is (= (freq-by identity :nama datadiri) (freq-by identity ["wisnu" "peter" "donna" "davin"])) "fail di :nama")
    (is (= (freq-by identity :tinggi datadiri) (freq-by identity [172 174 171 171])) "fail di :tinggi")
    (is (= (freq-by identity :berat datadiri) (freq-by identity [65 75 52 50])) "fail di :berat")
    (is (= (freq-by identity :fourclojure datadiri) (freq-by identity [88 132 108 98])) "fail di :fourclojure")
    (is (= (freq-by identity :kalogaada datadiri) :key-not-exist) "fail di :kalogaada")))

(deftest testing-quartile
  (testing "Testing Numeric Data"
    (is (= (quartile [1 2 3 4]) [3/2 5/2 7/2]) "fail nih 4 input")
    (is (= (quartile [1 2 3 4 5]) [3/2 3 9/2]) "fail nih 5 input")
    (is (= (quartile [1 1 1 2 2 3 3 4 4 4 4 5]) [3/2 3 4]) "fail nih...")
    (is (= (quartile [5 1 2 2 4 3 1 3 4 4 4 1]) [3/2 3 4]) "fail nih, datanya unsorted"))
  (testing "Testing Multi Dimentional Data"
    ;(is (= (quartile :nama datadiri) (quartile ["wisnu" "peter" "donna" "davin"])) "fail di :nama")
    (is (= (quartile :tinggi datadiri) (quartile [172 174 171 171])) "fail di :tinggi")
    (is (= (quartile :berat datadiri) (quartile [65 75 52 50])) "fail di :berat")
    (is (= (quartile :fourclojure datadiri) (quartile [88 132 108 98])) "fail di :fourclojure")
    (is (= (quartile :kalogaada datadiri) :key-not-exist) "fail di :kalogaada")))

(def datasembarang
  [{:sembarang 42} {:sembarang 42} {:sembarang 42} {:sembarang 45} {:sembarang 46}
   {:sembarang 49} {:sembarang 52} {:sembarang 52} {:sembarang 53} {:sembarang 57} {:sembarang 62}])

(deftest testing-decile
  (testing "Testing Numeric Data"
    (is (= (decile [1 2 3 4]) :coll-less-than-10) "fail nih untuk input kurang dari 10")
    (is (= (decile [23 25 25 26 27 28 28 31 32 32 32 40 41 41]) [24.0 25 26.5 28 29.5 32 32.0 40 41.0]
           ) "fail untuk input dari contoh yang ada di https://www.zenius.net/c/1374/data-tunggal-teori (video desil)")
    (is (= (decile [42 42 42 45 46 49 52 52 53 57 62]) [42.0 42.0 43.8 45.8 49 52.0 52.4 55.4 61.0]
           ) "fail untuk input dari contoh yang ada di https://www.zenius.net/c/1374/data-tunggal-teori (video desil)")
    (is (= (decile [32 25 32 23 27 28 28 32 41 31 40 41 25 26]) [24.0 25 26.5 28 29.5 32 32.0 40 41.0]
           ) "fail nih, datanya unsorted")
    (is (= (decile [42 42 42 45 46 49 52 52 53 57 62]) [42.0 42.0 43.8 45.8 49 52.0 52.4 55.4 61.0]
           ) "fail nih, datanya unsorted"))
  (testing "Testing Multi Dimentional Data"
    (is (= (decile :tinggi datadiri) (decile [172 174 171 171])) "fail di :tinggi")
    (is (= (decile :berat datadiri) (decile [65 75 52 50])) "fail di :berat")
    (is (= (decile :fourclojure datadiri) (decile [88 132 108 98])) "fail di :fourclojure")
    (is (= (decile :sembarang datasembarang) [42.0 42.0 43.8 45.8 49 52.0 52.4 55.4 61.0]) "fail untu datasembarang")
    (is (= (decile :kalogaada datadiri) :key-not-exist) "fail di :kalogaada")))
