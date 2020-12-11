(ns aoc-2020-7
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/input-7") #"\n"))

(defn input-build-contents [string]
  (let [no-trail (str/join (butlast string))
        split-cont (str/split no-trail #", ")
        cont-map (map (fn [x] (let [words (str/split x #" ")
                                    number (read-string (str (first words)))
                                    bag (str/join [(get words 1) (get words 2)])]
                                {(keyword bag) number})) split-cont)]
    (reduce conj cont-map)))

(defn build-map [input]
  (loop [map {}
         input input]
    (if (empty? input)
      map
      (let [split-1 (str/split (first input) #" contain ")
            bag-key (keyword (str/join (butlast (str/split (first split-1) #" "))))
            bag-contents (input-build-contents (second split-1))]
        (recur (conj map {bag-key bag-contents}) (rest input))))))

(defn find-contained [bag-map bag-key]
  (loop [bag-map bag-map result []]
    (if (empty? bag-map)
      result
      (recur (rest bag-map) (if (bag-key (second (first bag-map)))
                          (conj result (first (first bag-map)))
                          result)))))

(defn recursive-find-contained [bag-map bag-key]
  (let [keys-start (find-contained bag-map bag-key)]
    (loop [keys-total [] keys-current keys-start]
      (if (empty? keys-current)
        keys-total
        (recur (into keys-total keys-current)
               (reduce into (map (fn [x] (find-contained bag-map x))
                                  keys-current)))))))

(time (build-map input))
(time (count (distinct (recursive-find-contained (build-map input) :shinygold))))

;; part 2
(defn build-contents [bag-map search-key number]
  (if (contains? (search-key bag-map) :otherbags)
    {}
    (let [key-val-vec (map vector (keys (search-key bag-map))
                           (map (fn [x] (* x number))
                                (vals (search-key bag-map))))]
      (into {} key-val-vec))))

(defn recursive-build-contents [bag-map bag-key]
  (let [bags-start (bag-key bag-map)
        bag-count (apply + (vals bags-start))]
    (loop [bag-count bag-count bags-current bags-start]
      (if (empty? bags-current)
        bag-count
        (let [bag-vecs (map vector (keys bags-current) (vals bags-current))
              bags-now (reduce
                        (fn [a b] (merge-with + a b))
                        (map (fn [bag-vec] (build-contents
                                            bag-map
                                            (first bag-vec)
                                            (second bag-vec)))
                                   bag-vecs))]
          (recur (+ bag-count (apply + (vals bags-now)))
                 bags-now))))))

(recursive-build-contents (build-map input) :shinygold)
