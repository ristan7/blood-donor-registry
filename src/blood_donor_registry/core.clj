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

(defn donor->csv-row [donor]
  (str/join "," (map donor donor-keys)))

(defn donors->csv [donors]
  (str/join "\n" (map donor->csv-row donors)))

(defn save-donors! [file donors]
  (spit file (donors->csv donors)))

(defn find-donor [donors id]
  (some #(when (= id (:id %)) %) donors))

(defn append
  "Ignores an invalid donor or a taken id."
  [donors donor]
  (if (and (valid-donor? donor-validations donor)
           (nil? (find-donor donors (:id donor))))
    (conj (vec donors) donor)
    donors))

(defn load-donors
  "Skips invalid rows and repeated ids."
  [file]
  (->> (slurp file)
       parse
       (map row->donor)
       (reduce append [])))

(defn donors-by-blood-type [blood-type donors]
  (filter #(= blood-type (:blood-type %)) donors))

(defn top-donors [n donors]
  (take n (sort-by :donations > donors)))

(defn count-by-blood-type [donors]
  (frequencies (map :blood-type donors)))

(defn total-donations [donors]
  (reduce + (map :donations donors)))

(defn add-donation [donor]
  (update-in donor [:donations] inc))

(defn record-donation [donors id]
  (when (find-donor donors id)
    (mapv (fn [donor]
            (if (= (:id donor) id)
              (add-donation donor)
              donor))
          donors)))

(defn record-donation! [file id]
  (when-let [donors (record-donation (load-donors file) id)]
    (save-donors! file donors)
    donors))

(def rewards [{:donations 10 :reward "Zahvalnica"}
              {:donations 25 :reward "Srebrna značka"}
              {:donations 50 :reward "Zlatna značka"}
              {:donations 75 :reward "Plaketa"}
              {:donations 100 :reward "Povelja"}])

(defn earned-rewards [donations]
  (take-while #(>= donations (:donations %)) rewards))

(defn reward-for [donations]
  (last (earned-rewards donations)))

(defn donors-with-reward
  "Matches only the highest earned reward."
  [reward-name donors]
  (filter #(= reward-name (-> % :donations reward-for :reward)) donors))

(defn print-donors [donors]
  (doseq [donor donors]
    (println (:id donor)
             (:first-name donor)
             (:last-name donor)
             (:blood-type donor)
             (:donations donor))))

(defn -main [& args]
  (if (= "donate" (first args))
    (record-donation! donors-file (parse-long (second args)))
    (print-donors (load-donors donors-file))))
