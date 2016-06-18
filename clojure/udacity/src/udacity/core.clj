(ns udacity.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as mo]
            [clojure.core.matrix.stats :as stat]))

(m/set-current-implementation :vectorz)

(defn cslurp [filename]
  (->> (str "resources/" filename ".edn")
       slurp read-string))

(def data-1 (m/array :vectorz (cslurp "quiz1")))
(def data (cslurp "quiz1"))
(def data-fb (cslurp "fbfriends"))
(def data-rec (cslurp "recognitions"))
(def data-temp (cslurp "temporal"))
(def data-iq (cslurp "iq"))
(def z-table-data (cslurp "z-table"))

(defn mean [data]
  (/ (reduce + data)
     (double (count data))))

(defn avg-deviation [data]
  (let [avg (mean data)]
    (/ (->> (map #(- % avg) data)
            (reduce +))
       (count data))))

(defn sum-of-squared
  [data]
  (let [avg (mean data)]
    (transduce
      (map #(Math/pow (- % avg) 2))
      + data)))

(defn std-dev [data]
  (let [avg (mean data) size (count data)]
    (Math/sqrt
      (/ (->> data
              (map #(Math/pow (- % avg) 2))
              (reduce +))
         size))))

(defn sample-sd [data]
  (let [avg (mean data) size (count data)]
    (Math/sqrt
      (/ (->> data
              (map #(Math/pow (- % avg) 2))
              (reduce +))
         (dec size)))))

(defn variance [data]
  (let [avg (mean data)]
    (/ (->> data
            (map #(Math/pow (- % avg) 2))
            (reduce +))
       (count data))))

(defn z-score
  ([x avg sd]
   (/ (- x avg) sd))
  ([x data]
   (z-score x (mean data) (std-dev data))))

(defn majority [data]
  (let [sd (std-dev data)
        avg (mean data)
        ctr (count data)
        ctr-majority (count (filter #(<= (- avg sd) % (+ avg sd)) data))]
    [ctr ctr-majority (* 100.0 (/ ctr-majority ctr))]))

(defn roll [data]
  {:mean (mean data)
   :sd (std-dev data)
   :variance (variance data)
   :majority (majority data)})

(defn z-table
  "Z-table lookup function"
  [z-score]
  (cond
    (< z-score -3) 0.999
    (> z-score 3) 0.001
    :else (let [tab z-table-data]
            (if (pos? z-score)
              (tab (read-string (format "%.2f" z-score)))
              (- 1 (tab (read-string (format "%.2f" (Math/abs z-score)))))))))


