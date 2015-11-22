(ns wisnuops.core
  (:require
    [clojure.test :refer :all]
    [clojure.repl :refer :all]))

(defn means 
  ([coll] (/ (reduce + coll) (count coll)))
  ([key coll] (let [xs (map #(key % :key-not-exist) coll)] 
                (if (= (first xs) :key-not-exist) :key-not-exist (/ (reduce + xs) (count xs))))))

(defn freq 
  ([coll] 
    (apply hash-map 
           (mapcat #(vector (first %) (count %)) 
                   (partition-by identity (sort coll)))))
  ([key coll] 
    (let [xs (map #(key % :key-not-exist) coll)]
      (if (= (first xs) :key-not-exist) :key-not-exist
        (apply hash-map 
           (mapcat #(vector (first %) (count %)) 
                   (partition-by identity 
                                 (sort xs))))))))

(defn modes
  ([coll] (let [as (sort (vals (group-by identity coll)))
                n (count (last as))]
            (map first (filter #(= (count %) n) as))))
  ([key coll] (let [as (sort (vals (group-by identity (map #(key % :key-not-exist) coll))))
                n (count (last as))]
                (if (= (first (first as)) :key-not-exist) :key-not-exist
            (map first (filter #(= (count %) n) as))))))

(defn variance
  ([coll] 
    (let [xbar (means coll)] 
      (/ (reduce + (map #(* % %) (map #(- xbar %) coll))) (count coll))))
  ([key coll] 
    (let [xs (map #(key % :key-not-exist) coll)]
      (if (= (first xs) :key-not-exist) :key-not-exist
        (let [xbar (means xs)]
          (/ (reduce + (map #(* % %) (map #(- xbar %) xs))) (count xs)))))))

(defn std-dev
  ([coll] (Math/sqrt (variance coll)))
  ([key coll]
    (let [xs (map #(key % :key-not-exist) coll)]
      (if (= (first xs) :key-not-exist) :key-not-exist
        (Math/sqrt (variance xs))))))

(defn freq-by 
  ([f coll]
    (let [xmap (group-by f coll)]
      (apply hash-map
             (interleave
               (keys xmap)
               (map count (vals xmap))))))
  ([f key coll]
    (let [xs (map #(key % :key-not-exist) coll)]
      (if (= (first xs) :key-not-exist) :key-not-exist
        (let [xmap (group-by f xs)]
          (apply hash-map
             (interleave
               (keys xmap)
               (map count (vals xmap)))))))))

(defn median
  [coll]
  (if
    (odd? (count coll)) (first (drop (/ (- (count coll) 1) 2) (sort coll)))
    (let [xs (partition (/ (count coll) 2) (sort coll))]
      (means [(last (first xs)) (first (last xs))]))))

(defn quartile-2
  [coll]
    (median coll))

(defn quartile-1
  [coll]
  (if
    (odd? (count coll)) (median (take (/ (- (count coll) 1) 2) (sort coll)))
    (median (take (/ (count coll) 2) (sort coll)))))

(defn quartile-3
  [coll]
  (if
    (odd? (count coll)) (median (drop (/ (+ (count coll) 1) 2) (sort coll)))
    (median (drop (/ (count coll) 2) (sort coll)))))

(defn quartile
  ([coll] 
    [(quartile-1 coll) (quartile-2 coll) (quartile-3 coll)])
  ([key coll]
    (let [xs (map #(key % :key-not-exist) coll)]
      (if (= (first xs) :key-not-exist) :key-not-exist
        [(quartile-1 xs) (quartile-2 xs) (quartile-3 xs)]))))

(defn decile-i 
  [coll i]
  (if (> (count coll) 9)
    (let [n (- (/ (* i (inc (count coll))) 10) 1)]
      (if (= (class n) java.lang.Long) (nth (sort coll) n)
        (let [xlow (nth (sort coll) (int (Math/floor n)))
              xhigh (nth (sort coll) (int (Math/ceil n)))
              d (- n (Math/floor n))]
          (+ xlow (* d (- xhigh xlow))))))
    :coll-less-than-10))

(defn decile
  ([coll]
    (if (> (count coll) 9)
      [(decile-i coll 1) (decile-i coll 2) (decile-i coll 3) (decile-i coll 4) (decile-i coll 5)
       (decile-i coll 6) (decile-i coll 7) (decile-i coll 8) (decile-i coll 9)]
      :coll-less-than-10))
  ([key coll]
    (let [xs (map #(key % :key-not-exist) coll)]
      (if (= (first xs) :key-not-exist) :key-not-exist
        (if (> (count coll) 9)
          [(decile-i xs 1) (decile-i xs 2) (decile-i xs 3) (decile-i xs 4) (decile-i xs 5)
           (decile-i xs 6) (decile-i xs 7) (decile-i xs 8) (decile-i xs 9)]
         :coll-less-than-10)))))