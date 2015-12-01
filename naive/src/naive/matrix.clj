(ns naive.matrix)

(defn shape
  [v]
  (let [row (count v)
        col (count (first v))]
    (if (every? #(= col (count %)) v)
      [col row]
      (Throw. "It's not a matrix"))))

(defn m+
  "Addition of two matrices"
  [a b]
  {:pre [(= (shape a) (shape b))]})
