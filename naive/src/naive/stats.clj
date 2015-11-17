(ns naive.stats)

(defn means
  "Given one argument, returns the arithmetic means of all elements in
  the collection. Given two arguments, it returns the arithmetic means
  of all the keys supplied, the supplied keys (m-keys) must all have
  numeric values"
  ([col]
     (/ (reduce + col) (float (count col))))
  ([m-keys col]
     (let [res (map #(select-keys % m-keys) col)
           freq (float (count res))]
       (->> (zipmap m-keys (repeat freq))
            (merge-with / (reduce #(merge-with + % %2) res))))))

(defn freq
  "Given one argument, behaves exactly like clojure's frequencies.
  Given two arguments, it returns frequencies of each supplied key"
  ([col]
     (frequencies col))
  ([m-keys col]
     (->> (for [k m-keys]
            (frequencies (map k col)))
          (zipmap m-keys))))

(defn mode
  "Return the mode(s) of a data set. Given two arguments, it returns
  the mode for each supplied key."
  ([col]
     (->> (frequencies col)
          (apply max-key val)))
  ([m-keys col]
     (->> (for [k m-keys]
            (mode (map #(% k) col)))
          (zipmap m-keys))))

(defn tile
  "Returns the n-tile of a data. Given three arguments, it returns the
  vector of n-tile for each of the supplied key."
  ([n col]
     (let [ctr (count col)
           nths (map #(* % (quot ctr n)) (range 1 n))
           sorted (sort col)]
       (mapv #(nth sorted %) nths)))
  ([n m-keys col]
     (let [ctr (count col)
           nths (map #(* % (quot ctr n)) (range 1 n))]
       (->> (for [k m-keys]
              (let [sorted (sort (map #(% k) col))]
                {k (mapv #(nth sorted %) nths)}))
            (reduce merge)))))






