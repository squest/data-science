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

(defn- column-index
  "Helper function to get the index of a column in ds"
  [ds-columns column-name]
  (->> (map-indexed #(do [% %2]) ds-columns)
       (filter #(= (second %) column-name))
       ffirst))

(defn- column-vals
  "Helper function to get the values of a column in ds"
  [ds column-name]
  (let [idx (column-index (get ds :column-names) column-name)]
    (get-in ds [:columns idx])))

;; Implementations for freq
(declare freq-impl-ds freq-impl-ds-1 freq-impl-maps freq-impl-maps-1)

(defn freq
  "When given one argument, it assumes a single dimensional data, and
  returns the result of clojure's frequencies function.
  Given two arguments, the first one can be a collection of maps or
  datasets. Either way it returns the frequencies of each column."
  ([coll] (frequencies coll))
  ([keys-or-columns coll-or-ds]
   (cond
     (mds/dataset? coll-or-ds)
     (freq-impl-ds coll-or-ds keys-or-columns)
     :else
     (freq-impl-maps-1 coll-or-ds keys-or-columns))))

(declare freq-by-impl-ds-f freq-by-impl-maps-f
         freq-by-impl-ds-fs freq-by-impl-maps-fs)

(defn freq-by
  ([f coll] (frequencies (map f coll)))
  ([f-or-fs keys-or-columns coll-or-ds]
   (if (mds/dataset? coll-or-ds)
     (if (coll? f-or-fs)
       (freq-by-impl-ds-fs f-or-fs keys-or-columns coll-or-ds)
       (freq-by-impl-ds-f f-or-fs keys-or-columns coll-or-ds))
     (if (coll? f-or-fs)
       (freq-by-impl-maps-fs f-or-fs keys-or-columns coll-or-ds)
       (freq-by-impl-maps-f f-or-fs keys-or-columns coll-or-ds)))))

;; List of implementations

(defn- freq-by-impl-ds-f
  [f cols ds]
  (->> (map #(map f (column-vals ds %)) cols)
       (map frequencies)
       (zipmap cols)))

(defn- freq-impl-ds
  [cols ds]
  (->> (map #(column-vals ds %) cols)
       (map frequencies)
       (zipmap cols)))

(defn- freq-impl-ds-1
  [cols ds]
  (->> (mds/to-map ds)
       ((apply juxt cols))
       (map frequencies)
       (zipmap cols)))

(defn- freq-impl-maps
  [ks maps]
  (->> (for [k ks]
         (->> (map #(get % k) maps)
              frequencies))
       (zipmap ks)))

(defn- freq-impl-maps-1
  [ks maps]
  (->> (for [k ks]
         (loop [[x & xs] maps res (transient [])]
           (if x
             (recur xs (conj! res (get x k)))
             (frequencies (persistent! res)))))
       (zipmap ks)))






















