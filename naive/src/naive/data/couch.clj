(ns naive.data.couch
  (:require
   [com.ashafa.clutch :as cl]
   [clojure.core.async
    :refer [go go-loop <! >! <!! >!! alts! chan close! timeout]]
   [cemerick.url :refer [ url]]))


(def cdb
  (-> (url "https://zenius.cloudant.com/" "zenleague-logger")
      (assoc :username "whedesedstalleanithercie"
             :password "fb3eac61680496859fa7b30f103a3ba426cc69cc")))

(defn add-user []
  (let [nama (->> "qwertyuiopasdfghjklzxcvbnm"
                  seq shuffle
                  (take (rand-int 10))
                  (apply str))
        umur (rand-int 100)]
    (cl/put-document cdb {:username nama
                          :age umur
                          :type "test-user"})))

(defn all-users []
  (->> (cl/get-view cdb "UserTest" "byUsername")
       (map :value)))


