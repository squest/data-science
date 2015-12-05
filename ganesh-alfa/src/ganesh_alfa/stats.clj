(ns ganesh-alfa.stats
  (:require
    [incanter.core :as i]
    [incanter.charts :as ic]
    [incanter.datasets :as ds]
    [incanter.stats :as is]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as mds]
    [clojure.core.matrix.linear :as alin]
    [clojure.core.matrix.select :as sel]
    [clojure.core.matrix.stats :as stat]))

(defn column-index
  [ds-columns column-name]
  (->> (map-indexed #(do [% %2]) ds-columns)
       (filter #(= (second %) column-name))
       ffirst))

(defn column-vals
  [ds column-name]
  (let [idx (column-index (get ds :column-names) column-name)]
    (get-in ds [:columns idx])))

(declare freq-impl-ds freq-impl-maps)

(defn freq
  "When given one argument, it assumes a single dimensional data, and
  returns the result of clojure's frequencies function.
  Given two arguments, the first one can be a collection of maps or
  datasets. Either way it returns the frequencies of each column."
  ([coll] (frequencies coll))
  ([coll-or-ds keys-or-columns]
   (cond
     (mds/dataset? coll-or-ds)
     (freq-impl-ds coll-or-ds keys-or-columns)
     :else
     (freq-impl-maps coll-or-ds keys-or-columns))))

(defn freq-impl-ds
  [ds cols]
  (->> (map #(column-vals ds %) cols)
       (map frequencies)
       (zipmap cols)))






















