(ns advent-of-code.day-02
  (:require [clojure.string :as str])
  (:require [clojure.core.match :as match]))

(defn- calculateHorizontal [movements]
  (if-let [movement (first movements)]
    (let [direction (first movement)
          magnitude (Integer/parseInt (second movement))
          remainingMovements (rest movements)]
      (cond
        (= 0 (compare direction "forward")) (+ magnitude (calculateHorizontal remainingMovements))
        :else (calculateHorizontal (rest movements))))
    0))

(defn- calculateDepth [movements]
  (if-let [movement (first movements)]
    (let [direction (first movement)
          magnitude (Integer/parseInt (second movement))
          remainingMovements (rest movements)]
      (cond
        (= 0 (compare direction "down")) (+ (calculateDepth remainingMovements) magnitude)
        (= 0 (compare direction "up")) (- (calculateDepth remainingMovements) magnitude)
        :else (calculateDepth (rest movements))))
    0))

(defn- calculateDepthWithAim [movements aim]
  (if-let [movement (first movements)]
    (let [direction (first movement)
          magnitude (Integer/parseInt (second movement))
          remainingMovements (rest movements)]
      (match/match direction
        "forward" (+ (* magnitude aim) (calculateDepthWithAim remainingMovements aim))
        "down" (calculateDepthWithAim remainingMovements (+ aim magnitude))
        "up" (calculateDepthWithAim remainingMovements (- aim magnitude))))
    0))

(defn part-1
  "Day 02 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(str/split % #" ") $)
        (* (calculateHorizontal $) (calculateDepth $))))

(defn part-2
  "Day 02 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(str/split % #" ") $)
        (* (calculateHorizontal $) (calculateDepthWithAim $ 0))))
