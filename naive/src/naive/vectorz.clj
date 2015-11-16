(ns naive.vectorz)

(defn dot
  "Returns the dot product of two n-dimensional vectors"
  [v1 v2]
  (reduce + (map + v1 v2)))

