(ns advent-of-code.day-11
  (:require [clojure.string :as str]))

(defn- readNumbers [data]
  (->> data
       (re-seq #"\d")
       (map #(Integer/parseInt %))))

(defn- countOfOverNines [flashCounts]
  (->> flashCounts
      (flatten)
      (filter #(> % 9))
      (count)))

(defn- countOfNegatives [flashCounts]
  (->> flashCounts
       (flatten)
       (filter #(< % 0))
       (count)))

(defn- flashOctopus [octopi row index]
  (let [rowToTop (dec row)
        rowToBottom (inc row)
        indexToLeft (dec index)
        indexToRight (inc index)
        octopiVec (->> octopi
                       (map #(vec %))
                       (vec))]
    (as-> octopiVec $
          (if (and (>= rowToTop 0) (>= indexToLeft 0))
            (assoc-in $ [rowToTop indexToLeft] (inc (nth (nth $ rowToTop) indexToLeft))) $)
          (if (>= rowToTop 0)
            (assoc-in $ [rowToTop index] (inc (nth (nth $ rowToTop) index))) $)
          (if (and (>= rowToTop 0) (<= indexToRight 9))
            (assoc-in $ [rowToTop indexToRight] (inc (nth (nth $ rowToTop) indexToRight))) $)
          (if (and (<= rowToBottom 9) (>= indexToLeft 0))
            (assoc-in $ [rowToBottom indexToLeft] (inc (nth (nth $ rowToBottom) indexToLeft))) $)
          (if (<= rowToBottom 9)
            (assoc-in $ [rowToBottom index] (inc (nth (nth $ rowToBottom) index))) $)
          (if (and (<= rowToBottom 9) (<= indexToRight 9))
            (assoc-in $ [rowToBottom indexToRight] (inc (nth (nth $ rowToBottom) indexToRight))) $)
          (if (>= indexToLeft 0)
            (assoc-in $ [row indexToLeft] (inc (nth (nth $ row) indexToLeft))) $)
          (if (<= indexToRight 9)
            (assoc-in $ [row indexToRight] (inc (nth (nth $ row) indexToRight))) $)
          (assoc-in $ [row index] -10))))

(defn- zeroAndInc [octopi]
  (loop [row 0
         index 0
         flashCounts octopi]
    (if (> row (dec (count octopi)))
      flashCounts
      (recur (if (= index 9) (inc row) row)
             (if (= index 9) 0 (inc index))
             (if (> (nth (nth flashCounts row) index) 9)
               (flashOctopus flashCounts row index)
               flashCounts)))))

(defn- negToZero [list]
  (map #(if (< % 0) 0 %) list))

(defn- flash [octopi]
  (let [incrementedFlashes (hash-map :octopi (map #(map inc %) (:octopi octopi))
                                     :flashes (:flashes octopi))]
    (loop [flashes incrementedFlashes]
      (let [countOverNines (countOfOverNines (:octopi flashes))
            zeroedAndIncOctopi (zeroAndInc (:octopi flashes))]
        (if (zero? countOverNines)
          (hash-map :octopi (map negToZero (:octopi flashes))
                    :flashes (+ (:flashes octopi)
                                (countOfNegatives (:octopi flashes))))
          (recur (hash-map :octopi
                           zeroedAndIncOctopi
                           :flashes count)))))))

(defn- simulateFlashes [octopiAndCounts days]
  (loop [flashesAndCounts octopiAndCounts
         day 1]
    (if (> day days)
      flashesAndCounts
      (recur (flash flashesAndCounts) (inc day)))))

(defn- firstSimulFlash [octopiAndCounts]
  (loop [flashesAndCounts octopiAndCounts
         day 0]
    (if (= (->> (:octopi flashesAndCounts)
                (flatten)
                (filter #(= % 0))
                (count)) (count (flatten (:octopi flashesAndCounts))))
      day
      (recur (flash flashesAndCounts) (inc day)))))

(defn part-1
  "Day 11 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (map #(vec %) $)
        (vec $)
        (hash-map :octopi $ :flashes 0)
        (simulateFlashes $ 100)
        (:flashes $)))

(defn part-2
  "Day 11 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (map #(vec %) $)
        (vec $)
        (hash-map :octopi $ :flashes 0)
        (firstSimulFlash $)))
