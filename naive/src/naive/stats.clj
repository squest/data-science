(ns naive.stats)

(defn means
  "Given one argument, returns the arithmetic means of all elements in
  the collection. Given two arguments, it returns the arithmetic means
  of all the keys supplied, the supplied keys (m-keys) must all have
  numeric values"
  ([col]
     (/ (reduce + col) (float (count col))))
  ([col m-keys]
     (let [res (map #(select-keys % m-keys) col)
           freq (float (count res))]
       (->> (zipmap m-keys (repeat freq))
            (merge-with / (reduce #(merge-with + % %2) res))))))

(defn freq
  "Given one argument, behave exactly like clojure's frequencies.
  Given two arguments, it returns frequencies of each supplied key"
  ([col]
     (frequencies col))
  ([col m-keys]
     (->> (for [k m-keys]
            (frequencies (map k col)))
          (zipmap m-keys))))

(defn mode
  "Return the mode(s) of a data set. Given two arguments, it returns
  the mode for each supplied key."
  ([col]
     (->> (frequencies col)
          (max-key val)))
  ([col m-keys]
     (->> (for [k m-keys]
            (mode (map #(% k) col)))
          (zipmap m-keys))))
