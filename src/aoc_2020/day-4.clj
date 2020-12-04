(ns aoc-2020-4
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/input-4") #"\n\n"))

(defn format [input]
  (let [clear-breaks
        (map (fn [string] (str/replace string #"\n" " ")) input)]
    clear-breaks))
                      
(defn validate-pp [passport]
  (let [field-names ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]] ;; not sure why this has to be
    (every? true?                                                ;; a vector instead of a list
     (map (fn [str] (str/includes? passport str)) field-names))))

(defn parse-pps
  ([pp-list validator]
   (parse-pps pp-list validator 0))
  ([pp-list validator valid-count]
   (if (nil? (first pp-list))
     valid-count
     (let [valid-count (if (validator (first pp-list))
                         (inc valid-count)
                         valid-count)]
       (parse-pps (rest pp-list) validator valid-count)))))

(parse-pps (format input) validate-pp) ;; => 206

(def rx-map {:byr #"byr:(19[2-9][0-9]|200[0-2])(\ |$)"
             :iyr #"iyr:(201[0-9]|2020)(\ |$)"
             :eyr #"eyr:(202[0-9]|2030)(\ |$)"
             :hgt #"hgt:(((59|6[0-9]|7[0-6])in)|(1([5-8][0-9]|9[0-3])cm))(\ |$)"
             :hcl #"hcl:#[a-f0-9]{6}(\ |$)"
             :ecl #"ecl:(amb|blu|brn|gry|grn|hzl|oth)(\ |$)"
             :pid #"pid:[0-9]{9}(\ |$)"
             })

(defn validate-pp-2 [passport]
  (every? true?
    (map (fn [key] (if (nil? (re-find (key rx-map) passport))
                       false
                       true))
         (keys rx-map))))
(validate-pp-2 one-pp)

(last (format input))
(take 5 (format input))
(re-find (:byr rx-map) one-pp)
;; => ["byr:1969 " "1969" " "]

(parse-pps (format input) validate-pp-2) ;; => 99 - wrong










(parse-pps-2 (rest (rest (format input))))
(count (format input))
(parse-pps-2 (format input) 0)

(def one-pp (first (format input)))

;; => "ecl:#eef340 eyr:2023 hcl:#c0946f pid:244684338 iyr:2020 cid:57 byr:1969 hgt:152cm"
(last (format input))
(validate-pp-2 (last (format input)))









(keys {:key 1 :yek 2})

(second (format input))
(re-find (:byr rxmap) (first (format input)))
(re-matches #"(^|\ )byr:((19[2-9][0-9])|(200[0-2]))(\ |$)" (first (format input)))
(re-find #"(^|\ )byr:((19[2-9][0-9])|(200[0-2]))(\ |$)" (second (format input)))

              ;(^|\ )byr:(19[2-9][0-9])|(200[0-2])(\ |$)
(re-find #"\ byr:((19[2-9][0-9])|(200[0-2]))\ " (first (format input)))

(def rx-map {:byr #"byr:(19[2-9][0-9]|200[0-2])(\ |$)"
             :iyr #"iyr:(201[0-9]|2020)(\ |$)"
             :eyr #"eyr:(202[0-9]|2030)(\ |$)"
             :hgt #"hgt:((59|6[0-9]|7[0-6])in)|(1([5-8][0-9]|9[0-3])cm)(\ |$)"
             :hcl #"hcl:#[a-f0-9]{6}(\ |$)"
             :ecl #"ecl:(amb|blu|brn|gry|grn|hzl|oth)(\ | $)"
             :pid #"pid:[0-9]{9}(\ |$)"})
