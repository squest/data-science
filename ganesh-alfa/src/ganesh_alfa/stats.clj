(ns ganesh-alfa.stats
  (:require
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

;; Implementations details for freq
(defn- freq-impl-maps
  [ks maps]
  (->> (for [k ks]
         (->> (map #(get % k) maps)
              (frequencies)))
       (zipmap ks)))

(defn- freq-impl-ds
  [cols ds]
  (->> (map #(column-vals ds %) cols)
       (map frequencies)
       (zipmap cols)))

(defn freq
  "When given one argument, it assumes a single dimensional data, and
  returns the result of clojure's frequencies function.
  Given two arguments, the first one can be a collection of maps or
  datasets. Either way it returns the frequencies of each column."
  ([coll] (frequencies coll))
  ([keys-or-columns coll-or-ds]
     (if (mds/dataset? coll-or-ds)
       (freq-impl-ds keys-or-columns coll-or-ds)
       (freq-impl-maps keys-or-columns coll-or-ds))))

;; Implementations for freq-by

(defn- freq-by-impl-ds-f
  [f cols ds]
  (->> (map #(map f (column-vals ds %)) cols)
       (map frequencies)
       (zipmap cols)))

(defn- freq-by-impl-ds-fs
  [fs cols ds]
  (->> (map #(map (get fs %) (column-vals ds %)) cols)
       (map frequencies)
       (zipmap cols)))

(defn- freq-by-impl-maps-fs
  [fs ks maps]
  (->> (for [k ks]
         (->> (map #((get fs k) (get % k)) maps)
              frequencies))
       (zipmap ks)))

(defn- freq-by-impl-maps-f
  [f ks maps]
  (->> (for [k ks]
         (->> (map #(f (get % k)) maps) 
              frequencies))
       (zipmap ks)))

(defn freq-by
  "When given one argument it returns the result of invoking
  (frequencies (map f col)).
  When given two arguments, it will do exactly like one arg but
  applying it to all keys in each map or for every column for dataset.
  The collection can be either list of maps or core.matrix's dataset."
  ([f coll] (frequencies (map f coll)))
  ([f-or-fs keys-or-columns coll-or-ds]
   (if (mds/dataset? coll-or-ds)
     (if (coll? f-or-fs)
       (freq-by-impl-ds-fs f-or-fs keys-or-columns coll-or-ds)
       (freq-by-impl-ds-f f-or-fs keys-or-columns coll-or-ds))
     (if (coll? f-or-fs)
       (freq-by-impl-maps-fs f-or-fs keys-or-columns coll-or-ds)
       (freq-by-impl-maps-f f-or-fs keys-or-columns coll-or-ds)))))






















