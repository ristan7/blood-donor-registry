(ns blood-donor-registry.core-test
  (:require [blood-donor-registry.core :as core]
            [clojure.string :as str]
            [midje.sweet :refer [=> contains fact facts just provided]]))

(def marko {:id         1
            :first-name "Marko"
            :last-name  "Petrovic"
            :blood-type "A+"
            :donations  12})

(def jelena {:id         2
             :first-name "Jelena"
             :last-name  "Nikolic"
             :blood-type "0-"
             :donations  3})

(def stefan {:id         3
             :first-name "Stefan"
             :last-name  "Jovanovic"
             :blood-type "B+"
             :donations  27})

(def ana {:id         4
          :first-name "Ana"
          :last-name  "Ilic"
          :blood-type "AB+"
          :donations  51})

(def donors [marko jelena stefan ana])

(defn valid? [donor]
  (core/valid-donor? core/donor-validations donor))

(defn temp-file []
  (let [file (java.io.File/createTempFile "donors" ".csv")]
    (.deleteOnExit file)
    file))

(facts "about loading donors from CSV"
  (fact "convert parses numeric fields and keeps text fields"
    (core/convert :id "7") => 7
    (core/convert :donations "12") => 12
    (core/convert :first-name "Marko") => "Marko")

  (fact "parse splits text into rows of columns"
    (core/parse "1,Marko\n2,Jelena") => [["1" "Marko"] ["2" "Jelena"]])

  (fact "row->donor builds a donor map"
    (core/row->donor ["1" "Marko" "Petrovic" "A+" "12"]) => marko)

  (fact "load-donors reads and converts every line of the file"
    (core/load-donors "donors.csv") => [marko jelena]
    (provided
      (slurp "donors.csv") => "1,Marko,Petrovic,A+,12\n2,Jelena,Nikolic,0-,3"))

  (fact "load-donors skips rows that are not valid donors"
    (core/load-donors "donors.csv") => [marko jelena]
    (provided
      (slurp "donors.csv") => (str "1,Marko,Petrovic,A+,12\n"
                                   "5,Mila,Savic,X+,4\n"
                                   "\n"
                                   "6,Ivan,Ivic,B-,abc\n"
                                   "7,Nina\n"
                                   "2,Jelena,Nikolic,0-,3")))

  (fact "load-donors keeps only the first row for a repeated id"
    (core/load-donors "donors.csv") => [marko jelena]
    (provided
      (slurp "donors.csv") => (str "1,Marko,Petrovic,A+,12\n"
                                   "2,Jelena,Nikolic,0-,3\n"
                                   "1,Mila,Savic,0+,4"))))

(facts "about donor validation"
  (fact "a complete donor is valid"
    (valid? marko) => true)

  (fact "unknown blood type is rejected"
    (valid? (assoc marko :blood-type "C+")) => false)

  (fact "blank names are rejected"
    (valid? (assoc marko :first-name "  ")) => false)

  (fact "negative or missing donations are rejected"
    (valid? (assoc marko :donations -1)) => false
    (valid? (dissoc marko :donations)) => false)

  (fact "id has to be a positive integer"
    (valid? (assoc marko :id 0)) => false
    (valid? (assoc marko :id nil)) => false)

  (fact "a first-time donor with zero donations is valid"
    (valid? (assoc marko :donations 0)) => true))

(facts "about appending donors"
  (let [mila {:id         5
              :first-name "Mila"
              :last-name  "Savic"
              :blood-type "0+"
              :donations  0}]
    (fact "a valid donor is appended at the end"
      (core/append donors mila) => (just [marko jelena stefan ana mila]))

    (fact "an invalid donor is not appended"
      (core/append donors (assoc mila :blood-type "X")) => donors)

    (fact "a donor with an existing id is not appended"
      (core/append donors (assoc mila :id 1)) => donors)))

(facts "about writing donors back to CSV"
  (fact "one donor becomes one CSV line"
    (core/donor->csv-row marko) => "1,Marko,Petrovic,A+,12")

  (fact "donors->csv is the inverse of parsing"
    (->> donors
         core/donors->csv
         core/parse
         (map core/row->donor)) => donors)

  (fact "save-donors! writes donors as CSV to the file"
    (core/save-donors! "out.csv" [marko jelena]) => nil
    (provided
      (spit "out.csv" "1,Marko,Petrovic,A+,12\n2,Jelena,Nikolic,0-,3") => nil))

  (fact "saved donors load back unchanged"
    (let [file (temp-file)]
      (core/save-donors! file donors)
      (core/load-donors file) => donors)))

(facts "about querying donors"
  (fact "find-donor returns the donor with the given id"
    (core/find-donor donors 3) => stefan
    (core/find-donor donors 99) => nil)

  (fact "donors-by-blood-type filters on blood type"
    (core/donors-by-blood-type "0-" donors) => [jelena]
    (core/donors-by-blood-type "AB-" donors) => empty?)

  (fact "top-donors returns the n donors with most donations"
    (core/top-donors 2 donors) => [ana stefan])

  (fact "query functions compose with ->>"
    (->> (conj donors (assoc ana :id 5 :blood-type "B+" :donations 80))
         (core/donors-by-blood-type "B+")
         (core/top-donors 1)
         (map :id)) => [5])

  (fact "count-by-blood-type counts donors per type"
    (core/count-by-blood-type (conj donors (assoc jelena :id 9)))
    => (contains {"0-" 2 "A+" 1}))

  (fact "total-donations sums every donation"
    (core/total-donations donors) => 93))

(facts "about recording donations"
  (fact "add-donation adds one donation"
    (core/add-donation jelena) => (assoc jelena :donations 4))

  (fact "donor gets one more donation"
    (core/record-donation donors 2)
    => [marko (assoc jelena :donations 4) stefan ana])

  (fact "donors stay a vector"
    (core/record-donation donors 2) => vector?)

  (fact "unknown id gives nil"
    (core/record-donation donors 99) => nil)

  (fact "recorded donation is saved to the file"
    (let [file (temp-file)]
      (core/save-donors! file donors)
      (core/record-donation! file 2)
      (core/find-donor (core/load-donors file) 2)
      => (assoc jelena :donations 4)))

  (fact "unknown id leaves the file as it was"
    (let [file (temp-file)]
      (core/save-donors! file donors)
      (core/record-donation! file 99)
      (core/load-donors file) => donors))

  (fact "record-donation! returns the donors it saved"
    (let [file (temp-file)]
      (core/save-donors! file donors)
      (core/record-donation! file 2)
      => [marko (assoc jelena :donations 4) stefan ana]
      (core/record-donation! file 99) => nil))

  (fact "valid-file? is false when a row is bad"
    (let [file (temp-file)]
      (core/save-donors! file donors)
      (core/valid-file? file) => true
      (spit file "1,Marko,Petrovic,A+,12\n5,Mila,Savic,a+,7")
      (core/valid-file? file) => false))

  (fact "bad row is not deleted when recording a donation"
    (let [file (temp-file)]
      (spit file "1,Marko,Petrovic,A+,12\n5,Mila,Savic,a+,7")
      (core/record-donation! file 1) => nil
      (slurp file) => "1,Marko,Petrovic,A+,12\n5,Mila,Savic,a+,7")))

(facts "about rewards"
  (fact "no reward below the first threshold"
    (core/reward-for 9) => nil
    (core/earned-rewards 3) => empty?)

  (fact "the threshold itself earns the reward"
    (:reward (core/reward-for 10)) => "Zahvalnica")

  (fact "the highest reached reward is returned"
    (:reward (core/reward-for 51)) => "Zlatna značka"
    (:reward (core/reward-for 250)) => "Povelja")

  (fact "earned-rewards lists every reached reward in order"
    (map :reward (core/earned-rewards 27)) => ["Zahvalnica" "Srebrna značka"])

  (fact "donors-with-reward finds donors by their highest reward"
    (core/donors-with-reward "Zahvalnica" donors) => [marko]
    (core/donors-with-reward "Povelja" donors) => empty?))

(fact "main runs with the donors file"
  (core/-main "donate" "2") => nil
  (provided
    (core/run core/donors-file ["donate" "2"]) => nil))

(fact "donor->line shows the donor and the reward"
  (core/donor->line marko) => "1 Marko Petrovic A+ 12 Zahvalnica"
  (core/donor->line jelena) => "2 Jelena Nikolic 0- 3")

(fact "run prints all donors"
  (let [file (temp-file)]
    (core/save-donors! file donors)
    (str/split-lines (with-out-str (core/run file [])))
    => ["1 Marko Petrovic A+ 12 Zahvalnica"
        "2 Jelena Nikolic 0- 3"
        "3 Stefan Jovanovic B+ 27 Srebrna značka"
        "4 Ana Ilic AB+ 51 Zlatna značka"]))

(fact "donate records a donation for the id"
  (core/run "donors.csv" ["donate" "2"]) => nil
  (provided
    (core/donate! "donors.csv" 2) => nil))

(fact "parse-id turns text into a number"
  (core/parse-id "2") => 2
  (core/parse-id "abc") => nil
  (core/parse-id nil) => nil)

(fact "donate without a good id prints how to use it"
  (str/split-lines (with-out-str (core/run "donors.csv" ["donate"])))
  => ["Upotreba: lein run donate <id>"]
  (str/split-lines (with-out-str (core/run "donors.csv" ["donate" "abc"])))
  => ["Upotreba: lein run donate <id>"])

(fact "donate! says who got the donation"
  (let [file (temp-file)]
    (core/save-donors! file donors)
    (str/split-lines (with-out-str (core/donate! file 2)))
    => ["Upisana donacija za Jelena Nikolic - ukupno 4"]))

(fact "donate! says when there is no donor"
  (let [file (temp-file)]
    (core/save-donors! file donors)
    (str/split-lines (with-out-str (core/donate! file 99)))
    => ["Ne postoji donor sa ID 99"]))

(fact "donate! warns when the file has bad rows"
  (let [file (temp-file)]
    (spit file "1,Marko,Petrovic,A+,12\n5,Mila,Savic,a+,7")
    (str/split-lines (with-out-str (core/donate! file 1)))
    => ["Fajl ima neispravne redove, donacija nije upisana"]))
