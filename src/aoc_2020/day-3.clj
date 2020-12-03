(ns aoc-2020-2
  (:require [clojure.string :as str]))
(def input (str/split (slurp "resources/input-3") #"\n"))
(defn tree? [character]
  (if (= character \#)
    true
    false))
(get (get input 0) 37)

(defn fly
  ([input d-x d-y]
   (fly input d-x d-y 0 0 0))
  ([input d-x d-y pos-x pos-y tree-count]
   (let [width (count (first input))
         pos-x (if (>= pos-x width)
                 (mod pos-x width)
                 pos-x)
         tree-count (if (tree? (get (get input pos-y) pos-x))
                      (inc tree-count)
                      tree-count)]
     (if (nil? (get input pos-y))
       tree-count
       (fly input d-x d-y (+ pos-x d-x) (+ pos-y d-y) tree-count)))))

(fly input 3 1)

(* (fly input 1 1) (fly input 3 1) (fly input 5 1) (fly input 7 1) (fly input 1 2))
