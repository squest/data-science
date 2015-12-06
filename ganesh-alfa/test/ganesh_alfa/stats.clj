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
  (let [maxi 100
        single-data (->> (fn [] (rand-int 10))
                         (repeatedly maxi)
                         (filter prime?))
        ndim-data (for [i single-data
                        j single-data]
                    [(* i j) (+ i j) (- i j) (+ 100 i (- 100 j))])
        ds-version (ds/dataset [:a :b :c :d] ndim-data)
        maps-version (mapv #(zipmap [:a :b :c :d] %) ndim-data)]
    
    ;; single-dimensional data test, nothing fancy just frequencies
    (testing "freq fn to one-dimensional data"
      (is (= (freq single-data)
             (frequencies single-data))))

    ;; n-dimensional data for both dataset and list of maps
    (testing "freq fn to n-dimensional data"
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

  (let [maxi 100
        single-data (->> (fn [] (rand-int 10))
                         (repeatedly maxi))
        primes (filter prime? single-data)
        count-primes (count primes)
        ndim-data (for [i single-data
                        j single-data]
                    [(* i j) (+ i j) (- i j) (+ 100 i (- 100 j))])
        ds-version (ds/dataset [:a :b :c :d] ndim-data)
        maps-version (mapv #(zipmap [:a :b :c :d] %) ndim-data)]

    ;; single-dimensional data test
    (testing "freq-by to one-dimensional data"
      (is (= {true count-primes false (- maxi count-primes)}
             (freq-by prime? single-data)))

      (is (= {true 50 false 50}
             (freq-by odd? (range 100))))

      (is (= {0 20 1 20 2 20 3 20 4 20}
             (freq-by #(rem % 5) (range 1 101)))))

    ;; n-dimensional data tests
    (testing "freq-by to n-dimensional data using one fn"
      (is (= {:a {true count-primes false (- maxi count-primes)}
              :b (let [ctr (count (filter prime? (range maxi)))]
                   {true ctr false (- maxi ctr)})}
             (let [dats (-> #(hash-map :a % :b %2)
                            (map single-data (range maxi)))]
               (freq-by prime? [:a :b] dats))))
      (is (= {:a {true count-primes false (- maxi count-primes)}
              :b (let [ctr (count (filter prime? (range maxi)))]
                   {true ctr false (- maxi ctr)})}
             (let [dats (->> (interleave single-data (range maxi))
                             (partition 2)
                             (ds/dataset [:a :b]))]
               (freq-by prime? [:a :b] dats)))))

    (testing "freq-by to n-dimensional data using one map of fs"
      (is (= {:a (let [ctr (->> primes
                                (filter prime?)
                                count)]
                   {true ctr false (- maxi ctr)}) 
              :b {true 50 false 50}}
             (let [dats (-> #(hash-map :a % :b %2 :c (+ % %2))
                            (map single-data (range maxi)))]
               (freq-by {:a prime? :b odd?} [:a :b] dats))))
      (is (= {:a {true count-primes false (- maxi count-primes)}
              :b {true 50 false 50}}
             (let [dats (->> (interleave single-data
                                         (range maxi)
                                         (range maxi))
                             (partition 3)
                             (ds/dataset [:a :b :c]))]
               (freq-by {:a prime? :b odd?} [:a :b] dats)))))))

