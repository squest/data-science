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

(defn mean [data]
  (/ (reduce + data)
     (count data)))

(defn std-dev [data]
  (let [avg (mean data) size (count data)]
    (Math/sqrt
      (/ (->> data
              (map #(Math/pow (- % avg) 2))
              (reduce +))
         size))))

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




