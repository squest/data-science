(ns naive.examples.alfa)

(defn polynom
  [a b]
  (* (- (* a a) (* 10 a) -100)
     (+ (* b b) (- b 15))))

(defn generate []
  (->> (for [a (range 10)
             b (range 20)
             :let [res (polynom a b)]
             :when (= 20 (+ a b))]
         [res a b])
       (sort-by first <)))
