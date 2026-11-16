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

(defn row->donor [row]
  (reduce (fn [donor [k v]] (assoc donor k (convert k v)))
          {}
          (map vector donor-keys row)))

(defn load-donors [file]
  (map row->donor (parse (slurp file))))

(def blood-types #{"0+" "0-" "A+" "A-" "B+" "B-" "AB+" "AB-"})

(defn non-blank-string? [s]
  (and (string? s) (not (str/blank? s))))

(def donor-validations {:id         pos-int?
                        :first-name non-blank-string?
                        :last-name  non-blank-string?
                        :blood-type blood-types
                        :donations  nat-int?})

(defn valid-donor? [validations donor]
  (every? (fn [[k valid?]] (valid? (get donor k)))
          validations))

(defn append
  "Adds `donor` to the end of `donors` if it is valid.
  An invalid `donor` is ignored and `donors` is returned unchanged."
  [donors donor]
  (if (valid-donor? donor-validations donor)
    (conj (vec donors) donor)
    donors))

(defn donor->csv-row [donor]
  (str/join "," (map donor donor-keys)))

(defn donors->csv [donors]
  (str/join "\n" (map donor->csv-row donors)))

(defn find-donor [donors id]
  (some #(when (= id (:id %)) %) donors))

(defn donors-by-blood-type [blood-type donors]
  (filter #(= blood-type (:blood-type %)) donors))

(defn top-donors [n donors]
  (take n (sort-by :donations > donors)))

(defn count-by-blood-type [donors]
  (frequencies (map :blood-type donors)))

(defn total-donations [donors]
  (reduce + (map :donations donors)))

(def rewards [{:donations 10 :reward "Zahvalnica"}
              {:donations 25 :reward "Srebrna značka"}
              {:donations 50 :reward "Zlatna značka"}
              {:donations 75 :reward "Plaketa"}
              {:donations 100 :reward "Povelja"}])

(defn earned-rewards
  "Returns all rewards reached by `donations`, lowest first.
  Relies on `rewards` being sorted by `:donations` in ascending order."
  [donations]
  (take-while #(>= donations (:donations %)) rewards))

(defn reward-for
  "Returns the highest reward earned with `donations`, or nil if none."
  [donations]
  (last (earned-rewards donations)))

(defn donors-with-reward
  "Returns the `donors` whose highest earned reward is `reward-name`."
  [reward-name donors]
  (filter #(= reward-name (-> % :donations reward-for :reward)) donors))

(defn -main [& _args]
  (println "Hello, World!"))
