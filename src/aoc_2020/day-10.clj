(ns aoc-2020-9
  (:require [clojure.string :as str]))

(def input (sort (vec (map read-string (str/split (slurp "resources/input-10") #"\n")))))

(defn part-1 [input]
  (let [frq (map
             (frequencies (map - input (apply conj [0] (butlast input))))
             [1 3])]
    (* (first frq) (inc (last frq)))))
(part-1 input)

(sort (build-tree (reverse [19 16 15 12 11 10 7 6 5 4 1 0])))
(def small (reverse [19 16 15 12 11 10 7 6 5 4 1 0]))
(reduce #(+ %1 %2) (map #(count (second %)) (build-tree small)))
(sort (build-tree middle))

(def zinput (vec (sort (apply conj [0 (+ (last input) 3)] input))))
(def empty-memo (assoc (vec (repeat (last zinput) 0)) 0 1))

(defn count-paths [input memo]
  (loop [input input memo memo]
    (if (empty? input)
      (last memo)
      (let [local-paths (apply + (subvec memo (max 0 (- (first input) 3)) (max 1 (first input))))
            memo (assoc memo (first input) local-paths)
            dumb (do (println local-paths (first input)) "dumb")]
        (count-paths (rest input) memo)))))

(count-paths zinput empty-memo)
