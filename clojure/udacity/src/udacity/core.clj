(ns udacity.core)

(defn cslurp [filename]
  (->> (str "resources/" filename ".edn")
       slurp read-string))




