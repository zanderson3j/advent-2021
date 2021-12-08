(ns advent-of-code.day-08
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn- defineFiveLengths [map fiveLengthSegments]
  (let [three (filter #(= (count (set/difference % (:1 map))) 3) fiveLengthSegments)
        twoAndFive (filter #(= (count (set/difference % (:1 map))) 4) fiveLengthSegments)
        two (filter #(= (count (set/difference % (:4 map))) 3) twoAndFive)
        five (filter #(= (count (set/difference % (:4 map))) 2) twoAndFive)]
    (-> map
        (assoc :3 (nth three 0))
        (assoc :2 (nth two 0))
        (assoc :5 (nth five 0)))))

(defn- defineSixLengths [map sixLengthSegments]
  (let [six (filter #(= (count (set/difference % (:1 map))) 5) sixLengthSegments)
        zeroAndNine (filter #(= (count (set/difference % (:1 map))) 4) sixLengthSegments)
        zero (filter #(= (count (set/difference % (:4 map))) 3) zeroAndNine)
        nine (filter #(= (count (set/difference % (:4 map))) 2) zeroAndNine)]
    (-> map
        (assoc :6 (nth six 0))
        (assoc :0 (nth zero 0))
        (assoc :9 (nth nine 0)))))

(defn- defineNumbers [segment]
  (as-> {} $
        (assoc $ :1 (nth (filter #(= (count %) 2) segment) 0))
        (assoc $ :7 (nth (filter #(= (count %) 3) segment) 0))
        (assoc $ :4 (nth (filter #(= (count %) 4) segment) 0))
        (assoc $ :8 (nth (filter #(= (count %) 7) segment) 0))
        (defineFiveLengths $ (filter #(= (count %) 5) segment))
        (defineSixLengths $ (filter #(= (count %) 6) segment))))

(defn- getDigits [digits segments]
  (loop [element 0
         numericalDigits []]
    (if (> element 3)
      numericalDigits
      (recur (inc element)
             (conj numericalDigits (cond
                                     (= (nth digits element) (:0 segments)) "0"
                                     (= (nth digits element) (:1 segments)) "1"
                                     (= (nth digits element) (:2 segments)) "2"
                                     (= (nth digits element) (:3 segments)) "3"
                                     (= (nth digits element) (:4 segments)) "4"
                                     (= (nth digits element) (:5 segments)) "5"
                                     (= (nth digits element) (:6 segments)) "6"
                                     (= (nth digits element) (:7 segments)) "7"
                                     (= (nth digits element) (:8 segments)) "8"
                                     :else "9"))))))

(defn part-1
  "Day 08 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(str/split % #" ") $)
        (map #(drop 11 %) $)
        (flatten $)
        (filter #(or (= (count %) 2)
                     (= (count %) 3)
                     (= (count %) 4)
                     (= (count %) 7)) $)
        (flatten $)
        (count $)))

(defn part-2
  "Day 08 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(str/split % #" ") $)
        (map #(hash-map :segments (take 10 %) :digits (drop 11 %)) $)
        (map #(assoc % :segments (map (fn [segment] (set segment)) (:segments %))) $)
        (map #(assoc % :digits (map (fn [digit] (set digit)) (:digits %))) $)
        (map #(assoc % :segments (defineNumbers (:segments %))) $)
        (map #(getDigits (:digits %) (:segments %)) $)
        (map #(reduce str %) $)
        (map #(Integer/parseInt %) $)
        (flatten $)
        (reduce + $)))
