(ns teraining.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn means
  ([coll] (float (/ (reduce + coll) (count coll))))
  ([keys maps] (let [select (map #(select-keys % keys) maps)
                     fre (int (count select))]
                 (->> (zipmap keys (repeat fre))
                      (merge-with / (reduce #(merge-with + % %2) select))
                      (vals)
                      (map int)
                      (zipmap keys)))))
