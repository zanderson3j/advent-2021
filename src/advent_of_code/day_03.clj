(ns advent-of-code.day-03
  (:require [clojure.string :as str]))

(defn- sumLists [first second]
  (loop [summedList []
         list1 first
         list2 second]
    (if (empty? list1)
      summedList
      (recur (conj summedList (+ (nth list1 0) (nth list2 0))) (rest list1) (rest list2)))))

(defn- getSums [data]
  (reduce sumLists data))

(defn- getGammaValue [data]
  (let [length (count data)
        summedData (getSums data)]
    (loop [binaryValue []
           counts summedData]
      (if (empty? counts)
        binaryValue
        (recur (conj binaryValue (if (> (nth counts 0) (/ length 2)) 1 0)) (rest counts))))))

(defn- getEpsilonValue [data]
  (let [length (count data)
        summedData (getSums data)]
    (loop [binaryValue []
           counts summedData]
      (if (empty? counts)
        binaryValue
        (recur (conj binaryValue (if (< (nth counts 0) (/ length 2)) 1 0)) (rest counts))))))

(defn- getOxygenValue [data]
  (loop [length (count data)
         remainingData data
         counts (getSums data)
         idx 0]
    (if (= (count remainingData) 1)
      (nth remainingData 0)
      (let [matchingValue (if (>= (nth counts idx) (/ length 2)) 1 0)
            filtertedData (filter #(= matchingValue (nth % idx)) remainingData)
            filteredLength (count filtertedData)
            filteredCounts (getSums filtertedData)]
        (recur filteredLength filtertedData filteredCounts (+ idx 1))))))

(defn- getCO2Value [data]
  (loop [length (count data)
         remainingData data
         counts (getSums data)
         idx 0]
    (if (= (count remainingData) 1)
      (nth remainingData 0)
      (let [matchingValue (if (< (nth counts idx) (/ length 2)) 1 0)
            filtertedData (filter #(= matchingValue (nth % idx)) remainingData)
            filteredLength (count filtertedData)
            filteredCounts (getSums filtertedData)]
        (recur filteredLength filtertedData filteredCounts (+ idx 1))))))

(defn- toBinary [list]
  (loop [binaryString ""
         dataRemaining list]
    (if (empty? dataRemaining)
      binaryString
      (recur (str binaryString (nth dataRemaining 0)) (rest dataRemaining)))))

(defn part-1
  "Day 03 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (mapv #(mapv (fn [^Character binary] (Character/getNumericValue ^Character binary)) %) $)
        (* (Integer/parseInt (toBinary (getGammaValue $)) 2)
           (Integer/parseInt (toBinary (getEpsilonValue $)) 2))))

(defn part-2
  "Day 03 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (mapv #(mapv (fn [^Character binary] (Character/getNumericValue ^Character binary)) %) $)
        (* (Integer/parseInt (toBinary (getOxygenValue $)) 2)
           (Integer/parseInt (toBinary (getCO2Value $)) 2))))
