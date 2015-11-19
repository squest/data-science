(ns teraining.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

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
     (= (count collys) 0) "Jangan goblok"
     (= (count collys) 1) "ngentot lo"
     :else (->> (map #(kalipangkat % 2) collys)
                (reduce +)
                (#(/ % (count collys)))
                float)))
  ([yg-pengen-diambil coll] (->> (for [k yg-pengen-diambil] (variance (map #(k %) coll)))
                                 (zipmap yg-pengen-diambil))))
