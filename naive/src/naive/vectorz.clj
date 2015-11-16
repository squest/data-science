(ns naive.vectorz)

(defn dot
  "Returns the dot product of two n-dimensional vectors"
  [v1 v2]
  (reduce + (map + v1 v2)))

(defn add
  "Returns the addition of all n-dimensional vectors"
  [& vs]
  (vec (reduce #(map + % %2) vs)))

(defn abs
  "Returns the scalar abs of a vector"
  [v]
  (Math/sqrt (map #(* % %) v)))

(defn points->vec
  "Returns the vector value from two n-dimensional points"
  [p1 p2]
  (mapv - p2 p1))

