(ns naive.test.helper)

(defn f=
  "helper function for floating-point equality"
  [x a]
  (< (- x 0.01) a (+ x 0.01)))
