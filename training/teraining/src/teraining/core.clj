(ns teraining.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))



(defn quartiles
  [coll]
  (let [median (fn median
          [coll]
          (if (even? (count coll))
            (->> (/ (+ (nth coll (/ (count coll) 2)) (dec (nth coll (/ (count coll) 2)))) 2)
                 float)
            (nth coll (/ (count coll) 2))))

        quartile1(fn quartile1
          [coll]
          (->> (/ (+ (nth coll (int (float (+ (/ (count coll) 4) (/ 1 4)))))
                     (nth coll (dec (float (+ (/ (count coll) 4) (/ 1 4))))))
                  2)
               float))

        quartil3(fn quartile3
          [coll]
          (float (/ (+ (nth coll (int (float (+ (* 3 (/ (count coll) 4)) (/ 3 4)))))
                       (nth coll (dec (int (float (+ (* 3 (/ (count coll) 4)) (/ 3 4)))))))
                    2)))]
    [(quartile1 coll) (median coll) (quartil3 coll)]))



(defn means
  ([coll] (int (/ (reduce + coll) (count coll))))
  ([keys maps] (let [select (map #(select-keys % keys) maps)
                     fre (int (count select))]
                 (->> (zipmap keys (repeat fre))
                      (merge-with / (reduce #(merge-with + % %2) select))
                      (vals)
                      (map int)
                      (zipmap keys)))))

(defn kalipangkat
  [x y]
  (reduce * (repeat y x)))

(defn variance
  ([collys] (cond
     (= (count collys) 0) "ばか"
     (= (count collys) 1) "おいしい"
     :else (->> (map #(kalipangkat % 2) collys)
                (reduce +)
                (#(/ % (count collys)))
                float)))
  ([yg-pengen-diambil coll] (->> (for [k yg-pengen-diambil] (variance (map #(k %) coll)))
                                 (zipmap yg-pengen-diambil))))

(defn standard-deviation
  ([x] (if (or (= (count x) 0)
           (= (count x) 1))
     "坂本"
     (->> (map #(- % (means x)) x)
          (map #(kalipangkat % 2))
          (reduce +)
          (#(/ % (count x)))
          Math/sqrt)))
  ([yg-pengen-diambil coll] (->> (for [s yg-pengen-diambil] (standard-deviation (map #(s %) coll)))
                                 (zipmap yg-pengen-diambil))))


(defn mode
  ([coll] (apply max-key val (frequencies coll)))
  ([yg-pengen-diliat sicol] (zipmap yg-pengen-diliat (for [p yg-pengen-diliat] (mode (map #(p %) sicol))))))


