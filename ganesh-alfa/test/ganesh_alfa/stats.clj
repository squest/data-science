(ns ganesh-alfa.test.stats
  (:require
   [clojure.test :refer :all]
   [ganesh-alfa.stats :refer :all]
   [clojure.core.matrix.dataset :as ds]))

(defn- prime?
  "Helper function for test"
  [^long p]
  (cond (< p 2) false
        (== p 2) true
        (even? p) false
        :else (let [lim (inc (int (Math/sqrt p)))]
                (loop [i (int 3)]
                  (if (> i lim)
                    true
                    (if (== 0 (rem p i))
                      false
                      (recur (+ 2 i))))))))

(deftest freq-test
  (testing "freq function both for single-dimensional and n-dims coll"
    (let [single-data (->> (fn [] (rand-int 10))
                           (repeatedly 100)
                           (filter prime?))
          ndim-data (for [i single-data
                          j single-data]
                      [(* i j) (+ i j) (- i j) (+ 100 i (- 100 j))])
          ds-version (ds/dataset [:a :b :c :d] ndim-data)
          maps-version (mapv #(zipmap [:a :b :c :d] %) ndim-data)]
      
      ;; single-dimensional data test, nothing fancy just frequencies
      (is (= (freq single-data)
             (frequencies single-data)))

      ;; n-dimensional data for both dataset and list of maps
      (is (= [:a :c] (keys (freq [:a :c] ds-version))))
      (is (= [:b :d] (keys (freq [:b :d] maps-version))))
      (is (= [(->> (map first ndim-data)
                   (frequencies))
              (->> (map second ndim-data)
                   (frequencies))]
             (-> (freq [:a :b] ds-version)
                 ((juxt :a :b)))))
      (is (= [(->> (map first ndim-data)
                   (frequencies))
              (->> (map second ndim-data)
                   (frequencies))]
             (-> (freq [:a :b] maps-version)
                 ((juxt :a :b)))))))

  (testing "freq-by function for single-dim and n-dim data"
    (let [maxi 100
          single-data (->> (fn [] (rand-int 10))
                           (repeatedly maxi))
          primes (count (filter prime? single-data))
          ndim-data (for [i single-data
                          j single-data]
                      [(* i j) (+ i j) (- i j) (+ 100 i (- 100 j))])
          ds-version (ds/dataset [:a :b :c :d] ndim-data)
          maps-version (mapv #(zipmap [:a :b :c :d] %) ndim-data)]

      ;; single-dimensional data test
      (is (= {true primes false (- maxi primes)}
             (freq-by prime? single-data)))

      (is (= {true 50 false 50}
             (freq-by odd? (range 100))))

      (is (= {0 20 1 20 2 20 3 20 4 20}
             (freq-by #(rem % 5) (range 1 101)))))))
