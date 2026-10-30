(ns blood-donor-registry.core
  (:require [clojure.string :as str])
  (:gen-class))

(def donors-file "resources/donors.csv")

(def donor-keys [:id :first-name :last-name :blood-type :donations])

(def conversions {:id         parse-long
                  :first-name identity
                  :last-name  identity
                  :blood-type identity
                  :donations  parse-long})

(defn convert [k v]
  ((get conversions k) v))

(defn parse [text]
  (map #(str/split % #",") (str/split-lines text)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))