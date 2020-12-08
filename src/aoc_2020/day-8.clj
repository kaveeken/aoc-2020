(ns aoc-2020-8
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/bb-8-l") #"\n"))

(defn parse-line [line]
  (let [split-line (str/split line #" ")
        instruction (first split-line)
        delta (read-string (second split-line))
        d-acc (if (= instruction "acc")
                delta
                0)
        jump (if (= instruction "jmp")
               delta
               1)]
    {:jump jump :d-acc d-acc}))

(defn boot
  ([input]
   (let [next (parse-line (first input))
         index (:jump next)
         acc (:d-acc next)
         jumps (if (> index 1) [0] [])
         nops (if (= acc 0) [0] [])
         indices-seen [0 index]]
     (boot input index acc indices-seen jumps nops)))
  ([input index acc indices-seen jumps nops]
   (if (or (some #(= index %) (butlast indices-seen))
           (nil? (get input index)))
     {:acc acc :jumps jumps :nops nops :term (if (nil? (get input index)) 0 1)}
     (let [next (parse-line (get input index))
           jumps (if (> (:jump next) 1)
                   (conj jumps index)
                   jumps)
           nops (if (= (:d-acc next) 0)
                  (conj nops index)
                  nops)
           index (+ index (:jump next))
           acc (+ acc (:d-acc next))
           indices-seen (conj indices-seen index)]
       (boot input index acc indices-seen jumps nops)))))

(time (:acc (boot input)))

(defn flip-instruction [instruction]
  (if (= (first (str/split instruction #" ")) "jmp")
    (str/replace instruction #"jmp" "nop")
    (str/replace instruction #"nop" "jmp")))

(defn unloop
 ([input]
  (let [test (boot input)
        to-flip (into (:jumps test) (:nops test))]
    (unloop input to-flip)))
  ([input to-flip]
   (let [input-ch (assoc input (first to-flip)
                         (flip-instruction (get input (first to-flip))))
         test (boot input-ch)]
     (if (= (:term test) 0)
       (:acc test)
       (unloop input (rest to-flip))))))

(time (unloop input))
