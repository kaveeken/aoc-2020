(ns aoc-2020-2
  (:require [clojure.string :as str]))

(defn count-char
  ([string character]
   (if (= (first string) character)
       (count-char (rest string) character 1)
       (count-char (rest string) character 0)))
  ([string character amount]
   (if (= (first string) character)
     (if (empty? (rest string))
       (inc amount)
       (count-char (rest string) character (inc amount)))
     (if (empty? (rest string))
       amount
       (count-char (rest string) character amount)))))


(defn test-line [line-str]
  (let [split (str/split line-str #": ")
        string (last split)
        character (last (first split))
        minmax (first (str/split (first split) #" "))
        min (read-string (first (str/split minmax #"-")))
        max (read-string (last (str/split minmax #"-")))
        char-count (count-char string character)]
    (if (and (>= char-count min) (<= char-count max))
        true
        false)))

(defn count-file
  ([input fun]
   (if (fun (first input))
     (count-file (rest input) fun 1)
     (count-file (rest input) fun 0)))
  ([input fun pass-count]
   (if (empty? input)
     pass-count
     (if (fun (first input))
       (count-file (rest input) fun (inc pass-count))
       (count-file (rest input) fun pass-count)))))
  


(def a-line "1-3 a: abcde")
(str/split a-line #": ")
(test-line a-line)
(def input (str/split (slurp "resources/input-2") #"\n"))
(count-file input test-line)
(test-line (second input))

;; part two
(defn xor [one two]
  (if (and one two)
    false
    (if (or one two)
      true
      false)))

(defn test-position [string character pos]
  (if (= (get string (dec pos)) character)
    true
    false))
(defn test-line-pos [line-str]
  (let [split (str/split line-str #": ")
        string (last split)
        character (last (first split))
        minmax (first (str/split (first split) #" "))
        pos1 (read-string (first (str/split minmax #"-")))
        pos2 (read-string (last (str/split minmax #"-")))
        test-1 (test-position string character pos1)
        test-2 (test-position string character pos2)]
    (if (xor test-1 test-2)
        true
        false)))

(count-file input test-line-pos)
