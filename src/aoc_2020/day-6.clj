(ns aoc-2020-6
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/input-6") #"\n\n"))

(defn count-unique
  ([string]
   (let [string (str/replace string #"\n" "")]
     (count-unique (rest string) [(first string)])))
  ([string char-vec]
   (if (empty? string)
     (count char-vec)
     (count-unique (rest string) 
                   (if (some #{(first string)} char-vec)
                     char-vec
                     (conj char-vec (first string)))))))

(defn sum-counts [fun input]
  (apply + (map fun input)))
         
(sum-counts count-unique input)
  
;; part two
(defn count-shared [string]
  (let [string (if (= (last string) \newline)
                 (str/join (butlast string))
                 string)
        people-count (count (str/split string #"\n"))
        char-counts (vals (frequencies (str/replace string #"\n" "")))
        char-pass-count (apply + (map
                                  (fn [x] (if (= x people-count) 1 0))
                                  char-counts))]
   char-pass-count))

(sum-counts count-shared input)
