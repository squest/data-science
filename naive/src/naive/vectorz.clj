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
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn points->vec
  "Returns the vector value from two n-dimensional points"
  [p1 p2]
  (mapv - p2 p1))

(defn scalar-times
  "Returns a scalar multiplication of vector v with constant k"
  [k v]
  (mapv * (repeat k) v))

(defn project
  "Returns the orthogonal projection of vector a in b. When given three
  arguments, it will return the vector version"
  ([a b]
     (/ (dot a b) (abs b)))
  ([a b k]
     (let [abs-b (abs b)
           abs-x (/ (dot a b) abs-b)]
       (scalar-times (/ abs-x abs-b) b))))
