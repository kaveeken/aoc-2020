(ns aoc-2020-1
  (:require [clojure.string :as str]))

(def input (map read-string (str/split (slurp "resources/input") #"\n")))

(defn test-two [list value]
  (if (empty? list)
    nil
    (if (= (+ (first list) value) 2020)
      (* (first list) value)
      (test-two (rest list) value))))

(defn part-one
  "returns the product of the (first?) pair of numbers in EXPS that sum 2020"
  ([exps]
   (part-one (rest exps) (first exps)))
  ([exps value]
   (let [product (test-two exps value)]
     (if (nil? product)
       (part-one (rest exps) (first exps))
       product))))

(part-one input)

(defn test-three [list val1 val2]
  (if (empty? list)
    nil
    (if (= (+ (first list) val1 val2) 2020)
      (* (first list) val1 val2)
      (test-three (rest list) val1 val2))))
(defn something [list val1]
  (let [product (test-three (rest list) val1 (first list))]
    (if (nil? product)
      (if (empty? (rest (rest list)))
        nil
        (something (rest list) val1))
      product)))

(defn part-two
  "returns the product of the (first?) set of three numbers in EXPS that sum 2020"
  ([exps]
   (part-two (rest exps) (first exps)))
  ([exps value]
   (let [product (something exps value)]
     (if (nil? product)
       (part-two (rest exps) (first exps))
       product))))

(part-two input)
