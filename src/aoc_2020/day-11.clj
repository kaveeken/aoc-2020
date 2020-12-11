(ns aoc-2020-11
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp]))

(def input (map #(str/split % #"") (str/split (slurp "resources/input-11") #"\n")))

(defn translate-grid [grid]
  (loop [grid grid new-grid []]
    (if (empty? grid) new-grid
        (let [new-row (map #(case %
                              "L" 0
                              "#" 1
                              "." nil)
                           (first grid))]
    (recur (rest grid) (reduce conj new-grid [(vec new-row)]))))))

(def number-grid  (translate-grid input))

(defn count-neighbours [grid x y]
  (let [addnil #(if (nil? %) 0 %)
        look (fn [x y dx dy]
               (loop [x x y y]
                 (let [new-x (+ x dx)
                       new-y (+ y dy)
                       value (get (get grid new-y) new-x)]
                   (if (or (> new-x (count (first grid)))
                           (< new-x 0)
                           (> new-y (count grid))
                           (< new-y 0))
                     0
                     (if (= value 1)
                       1
                       (if (= value 0)
                         0
                         (recur new-x new-y)))))))
        seen-seats {:north (look x y 0 -1)
                    :northeast (look x y 1 -1)
                    :east (look x y 1 0)
                    :southeast (look x y 1 1)
                    :south (look x y 0 1)
                    :southwest (look x y -1 1)
                    :west (look x y -1 0)
                    :northwest (look x y -1 -1)}]
    (reduce + (vals seen-seats))))

(defn grid-step [grid]
  (let [new-grid grid
        test #(let [value (get (get %1 %3) %2)
                    neighbours (count-neighbours %1 %2 %3)]
                (case value
                  nil nil
                  0 (if (= 0 neighbours) 1 0)
                  1 (if (>= (count-neighbours %1 %2 %3) 5) 0 1)))] 
    (loop [xs (range (count (first grid)))
           ys (range (count grid))
           new-grid grid]
      (if (empty? ys)
        new-grid
        (recur (if (empty? (rest xs))
                 (range (count (first grid)))
                 (rest xs))
               (if (empty? (rest xs))
                 (rest ys)
                 ys)
               (let [y (first ys) x (first xs)]
                 (assoc new-grid y (assoc (get new-grid y)  x
                                 (test grid x y)))))))))

(defn simulate [grid]
  (let [nilplus #(let [arg1 (if (nil? %1) 0 %1)
                       arg2 (if (nil? %2) 0 %2)]
                   (+ arg1 arg2))
        max-iter 10000]
    (loop [grid grid checksum -1 iter 0]
      (let [new-chum (do (println checksum)
                         (reduce + (map #(reduce nilplus %) grid)))]
        (if (or (= new-chum checksum) (>= iter max-iter)) 
          new-chum
          (recur (grid-step grid) new-chum (inc iter)))))))

(simulate number-grid)
