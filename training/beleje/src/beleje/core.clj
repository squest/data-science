(ns beleje.core)

(def data4
  [{:name "Jon Snow" :iq 100 :eq 160 :status "deceased"}
   {:name "Tyrion Lannister" :iq 150 :eq 130 :status "missing"}
   {:name "Daenarys Targaryen" :iq 130 :eq 100 :status "alive"}
   {:name "Sansa Stark" :iq 90 :eq 90 :status "missing"}
   {:name "Petyr Baelish" :iq 180 :eq 70 :status "alive"}
   {:name "Varys" :iq 170 :eq 150 :status "missing"}
   {:name "Ned Stark" :iq 120 :eq 140 :status "deceased"}
   {:name "Cersei Lannister" :iq 110 :eq 90 :status "alive"}])

;; helper functions

(defn mapping-data
  [f dakeys damaps]
  (let [a (fn [key] {key (f (mapv #(% key) damaps))})]
    (apply merge (map a dakeys))))

(defn nilai
  [danums]
  {:0-4  (count (filter #(<= (int %) 4) danums))
   :5-7  (count (filter #(<= 5 (int %) 7) danums))
   :8-10 (count (filter #(<= 8 (int %) 10) danums))})

(defn eq-iq-group
  ([danums]
   {:<=100   (count (filter #(<= % 100) danums))
    :101-130 (count (filter #(<= 101 % 130) danums))
    :>131    (count (filter #(>= % 131) danums))})
  ([dakeys damaps]
    (mapping-data eq-iq-group dakeys damaps)))

(defn survivor-count
  ([dastatuses]
   {:survivor (count (filter #(or (= % "missing") (= % "alive")) dastatuses))
    :stupid   (count (filter #(= % "deceased") dastatuses))})
  ([dakeys damaps]
    (mapping-data survivor-count dakeys damaps)))


;; main functions


(defn means
  ([danums]
   (/ (apply +' danums) (count danums)))
  ([dakeys damaps]
    (mapping-data means dakeys damaps)))

(defn mode
  ([danums]
   (let [data (frequencies danums)
         max-frequency (apply max (vals data))]
     (filterv #(if (= (data %) max-frequency) %) (keys data))))
  ([dakeys damaps]
    (mapping-data mode dakeys damaps)))

(defn variance
  ([danums]
   (let [sqr (fn [x] (* x x))]
     (apply +' (map #(sqr (- %1 %2)) danums (repeat (means danums))))))
  ([dakeys damaps]
    (mapping-data variance dakeys damaps)))

(defn std
  ([danums]
   (Math/sqrt (variance danums)))
  ([dakeys damaps]
    (mapping-data std dakeys damaps)))

(defn freq
  ([danums]
   (frequencies danums))
  ([dakeys damaps]
    (mapping-data freq dakeys damaps)))

(defn freq-by
  ([f datas]
   (f datas))
  ([f dakeys damaps]
    (f dakeys damaps)))

(defn quartile
  [danums]
  (let [data (vec (sort danums))
        total (count data)]))





















