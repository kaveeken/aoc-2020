(ns aoc-2020-9
  (:require [clojure.string :as str]))

(def input (vec (map read-string (str/split (slurp "resources/bb-9") #"\n"))))

(defn validate [previous current]
  (let [previous (sort previous)]
    (loop [previous previous]
      (if (empty? (rest previous))
        false
        (let [sum (+' (first previous) (last previous))]
          (if (= sum current)
            true
            (if (> sum current)
              (recur (butlast previous))
              (recur (rest previous)))))))))
              
(defn find-invalid [input]
  (let [preamble (subvec input 0 25)
        input (subvec input 25)]
    (loop [input input preamble preamble]
      (let [current (first input)]
        (if (not (validate preamble current))
          current
          (recur (rest input) (conj preamble current)))))))

(time (find-invalid input))
;; => 6609672557935966302174909N
;;"Elapsed time: 7552.612856 msecs"


(defn find-weakness [input]
  (let [target (find-invalid input)
        range (conj [] (get input 0))
        index 0]
    (loop [range range index index]
      (let [sum (apply +' range)]
        (if (= sum target)
          (let [range (sort range)]
          (+' (first range) (last range)))
          (let [range (if (> sum target)
                        (vec (rest range))
                        (conj range (get input (inc index))))
                index (if (> sum target)
                        index
                        (inc index))]
            (recur range index)))))))
          
(time (find-weakness input))
;; => 13219345115871932604349818N
;;"Elapsed time: 7684.711264 msecs"

