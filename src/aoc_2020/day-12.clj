(ns aoc-2020-11
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/input-12") #"\n"))
(defn deg2rad [degrees]
  (Math/toRadians degrees))

(defn get-direction [facing]
  {:d-x (int (Math/cos (deg2rad facing)))
   :d-y (int (Math/sin (deg2rad facing)))})

(def state-0 {:facing 0 :pos-x 0 :pos-y 0 :wp-x 10 :wp-y 1})

(defn step [state instruction]
  (let [new-state state
        size (read-string (str/join (rest instruction)))
        direction (get-direction (:facing state))]
    (case (first instruction)
      \N (assoc new-state :pos-y (+ (:pos-y state) size))
      \S (assoc new-state :pos-y (- (:pos-y state) size))
      \E (assoc new-state :pos-x (+ (:pos-x state) size))
      \W (assoc new-state :pos-x (- (:pos-x state) size))
      \L (assoc new-state :facing (mod (+ (:facing state) size) 360))
      \R (assoc new-state :facing (mod (- (:facing state) size) 360))
      \F (assoc
          (assoc new-state :pos-y
                 (+ (* (:d-y direction) size) (:pos-y state)))
          :pos-x (+ (* (:d-x direction) size) (:pos-x state))))))

(defn travel [step-fun input state]
  (loop [input input state state]
    (if (empty? input) (+ (Math/abs (:pos-x state))
                          (Math/abs (:pos-y state)))
    (recur (rest input) (step-fun state (first input))))))
(travel step input state-0)

;; part 2
(defn rotate-waypoint [state degrees]
  (let [tmp (get-direction degrees)
        cos (:d-x tmp)
        sin (:d-y tmp)
        new-x (- (* (:wp-x state) cos)
                 (* (:wp-y state) sin))
        new-y (+ (* (:wp-x state) sin)
                 (* (:wp-y state) cos))
        new-state (assoc (assoc state :wp-y new-y) :wp-x new-x)]
    new-state))
    
(defn step-2 [state instruction]
  (let [new-state state
        size (read-string (str/join (rest instruction)))]
    (case (first instruction)
      \N (assoc new-state :wp-y (+ (:wp-y state) size))
      \S (assoc new-state :wp-y (- (:wp-y state) size))
      \E (assoc new-state :wp-x (+ (:wp-x state) size))
      \W (assoc new-state :wp-x (- (:wp-x state) size))
      \L (rotate-waypoint new-state size)
      \R (rotate-waypoint new-state (* -1 size))
      \F (assoc
          (assoc new-state :pos-y
                 (+ (* (:wp-y state) size) (:pos-y state)))
          :pos-x (+ (* (:wp-x state) size) (:pos-x state))))))
(travel step-2 input state-0)
