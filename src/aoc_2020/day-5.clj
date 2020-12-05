(ns aoc-2020-5
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/input-5") #"\n"))
(def row-count 128)
(def col-count 8)
(defn compute-id
  ([bstring]
   (let [row-space (vec (range 128))
         col-space (vec (range 8))]
     (compute-id bstring col-space row-space)))
  ([bstring col-space row-space]
   (if (empty? bstring)
     (+ (* (first (vec row-space)) 8) (first (vec col-space)))
     (let [bit (first bstring)
           row-split (if (or (= bit \B) (= bit \F))
                       (split-at (/ (count row-space) 2) row-space)
                       (split-at (count row-space) row-space))
           row-space (if (= bit \B)
                       (second row-split)
                       (first row-split))
           col-split (if (or (= bit \R) (= bit \L))
                       (split-at (/ (count col-space) 2) col-space)
                       (split-at (count col-space) col-space))
           col-space (if (= bit \R)
                       (second col-split)
                       (first col-split))]
         (compute-id (rest bstring) col-space row-space)))))

(defn parse-passes
  ([input]
   (parse-passes (rest input) (compute-id (first input))))
  ([input largest]
   (if (empty? input)
     largest
     (let [new (compute-id (first input))
           largest (max largest new)]
       (parse-passes (rest input) largest))))) 

(parse-passes input)

;; part two
(def id-list (sort (map compute-id input)))
(defn find-seat
  ([id-list]
   (find-seat (rest id-list) (first id-list)))
  ([id-list prev-seat]
   (if (and (= (first id-list) (inc prev-seat))
            (= (first id-list) (dec (second id-list))))
     (find-seat (rest id-list) (first id-list))
     (+ (first id-list) 1))))
 ;; this is a Long all of a sudden??

(find-seat id-list)
