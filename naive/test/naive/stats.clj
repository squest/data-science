(ns naive.test.stats
  (:require
   [naive.stats :refer :all]
   [clojure.test :refer :all]))

(deftest testing-stats
  (let [single (for [i (range 1000)] (rand-int 50))
        multi (for [i (range 100)
                    j (range 100)
                    k (range 100)]
                (let [a (rand-int i)
                      b (rand-int j)
                      c (rand-int k)]
                  {:a a :b b :c c
                   :d (* a b)
                   :e "Mereka"
                   :f "PastiKamu"}))]

    ;; First, test the means
    (testing "Means for single and multiple"
      (is (= (/ (reduce + single) (count single) 1.0)
             (means single)))
      (let [{:keys [a d]} (means [:a :d] multi)]
        (is (= (means (mapv :a multi)) a))
        (is (= (means (mapv :d multi)) d)))
      (is (= (->> [:a :b :c :d]
                  (map #(means (map % multi)))
                  (zipmap [:a :b :c :d]))
             (means [:a :b :c :d] multi)))
      (is (== 10 (means (repeat 20 10))))
      (is (= {:a 10.0 :b 10.0 :c 10.0}
             (->> (map #(zipmap [:a :b :c] %)
                       (repeat 100 (repeat 3 10)))
                  (means [:a :b :c])))))

    ;; Next, test the frequencies
    (testing "Frequencies for single and multiple"
      (is (= (frequencies single)
             (freq single)))
      (let [{:keys [a b c d]} (freq [:a :b :c :d] multi)]
        (is (= (frequencies (map :a multi)) a))
        (is (= (frequencies (map :b multi)) b))
        (is (= (frequencies (map :c multi)) c))
        (is (= (frequencies (map :d multi)) d))
        (is (= {10 100 9 100}
               (freq (concat (repeat 100 10) (repeat 100 9)))))
        (is (= {:a {10 10 9 10} :b {10 10 9 10}}
               (->> (concat (repeat 10 10) (repeat 10 9))
                    (map #(hash-map :a % :b %))
                    (freq [:a :b]))))))

    ;; Next, testing mode
    (testing "Modes for single and multiple"
      (is (= [5 7]
             (mode [1 1 1 2 2 3 3 4 4 5 5 5 5 5 5 5])))
      (is (= {:a [3 4] :b [5 6]}
             (let [res (map #(hash-map :a % :b %2)
                            [1 1 2 2 3 3 3 3 4 4]
                            [1 1 2 5 5 5 5 5 5 3])]
               (mode [:a :b] res))))
      (is (= 5 (->> (repeatedly 100 #(rand-int 100))
                    (concat (repeat 100 5))
                    mode first)))
      (is (= {:a 5 :b 10}
             (-> (mode [:a :b]
                       (for [i (concat (repeat 20 5)
                                       (range 20))]
                         {:a i :b (* 2 i) :c "mereka" :d :kokom}))
                 (update-in [:a] first)
                 (update-in [:b] first)))))

    ;; Next, testing tile
    (testing "nth-tile for single and multiple"
      (is (= [10] (tile 2 [1 10 20])))
      (is (= {:a [10] :b [20]}
             (->> [{:a 1 :b 2} {:a 10 :b 20} {:a 20 :b 30}]
                  (tile 2 [:a :b])))))))








