(ns naive.test.vectorz
  (:require
   [naive.vectorz :refer :all]
   [naive.test.helper :refer :all]
   [clojure.test :refer :all]))

(deftest testing-vectorz
  (let [A [4 5 6] B [10 9 8]
        u [1 -2 3] v [4 10 -4]
        k 5 l -3.0]
    
    (testing "dot product"
      (is (= (reduce + (map * A B))
             (dot A B)))
      (is (= (apply + (map * u v))
             (dot u v))))

    (testing "additions"
      (is (= (map + A B)
             (add A B)))
      (is (= (map + u v)
             (add u v)))
      (is (= [0 0 0.0]
             (add [12 -9 3] [-12 9 -3.0]))))

    (testing "size of the vector"
      (is (= 5.0 (abs [3 0 4])))
      (is (= 10.0 (abs [10 0 0])))
      (is (= (Math/sqrt 120)
             (abs [10 -4 -2])))
      (is (f= (* 10 (Math/sqrt 3))
              (abs [10 10 10]))))

    (testing "points to vectors"
      (is (= [1 2 3]
             (points->vec [0 0 0] [1 2 3])))
      (is (= [0 0 0]
             (points->vec [10 10 10] [10 10 10])))
      (is (= [6 4 2]
             (points->vec A B)))
      (is (= [-10 -10 -10]
             (points->vec [5 5 5] [-5 -5 -5]))))

    (testing "scalar multiplication of vectors"
      (is (= [10 10 10]
             (scalar-times 2 [5 5 5])))
      (is (= [-2.0 -2.0 -2.0]
             (scalar-times 2 [-1.0 -1.0 -1.0])))
      (is (= [3.5 3.5 3.5]
             (scalar-times 1/2 [7.0 7.0 7.0]))))

    (testing "projections of vectors"
      (is (= 0 (project [1 2 3] [0 0 0])))
      (is (= 10.0 (project [10 3 2] [10 0 0])))
      (is (= [0 0 0] (project [1 2 3] [0 0 0] :t)))
      (is (= [10.0 0.0 0.0] (project [10 2 3] [10 0 0] :t))))))
