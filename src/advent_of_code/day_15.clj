(ns advent-of-code.day-15
  (:require [clojure.string :as str]))

(defn readNumbers [data]
  (->> data
       (re-seq #"\d")
       (map #(Integer/parseInt %))))

(defn returnZero [input]
  0)

(defn returnTrue [input]
  true)

(defn returnMaxVal [input]
  Integer/MAX_VALUE)

(defn returnFalse [input]
  false)

(defn get2DValue [grid row column]
  (nth (nth grid row) column))

(defn- upDateRight [])

(defn updateNeighboringDistances [weights distances visited row column maxRowIdx maxColIdx]
  (as-> distances $
        (if (and (< column maxColIdx)
                 (false? (get2DValue visited row (inc column))))
          (update-in $ [row (inc column)] #(min % (+ (get2DValue weights row (inc column))
                                                     (get2DValue $ row column))))
          $)
        (if (and (> column 0)
                 (false? (get2DValue visited row (dec column))))
          (update-in $ [row (dec column)] #(min % (+ (get2DValue weights row (dec column))
                                                     (get2DValue $ row column))))
          $)
        (if (and (< row maxRowIdx)
                 (false? (get2DValue visited (inc row) column)))
          (update-in $ [(inc row) column] #(min % (+ (get2DValue weights (inc row) column)
                                                     (get2DValue $ row column))))
          $)
        (if (and (> row 0)
                 (false? (get2DValue visited (dec row) column)))
          (update-in $ [(dec row) column] #(min % (+ (get2DValue weights (dec row) column)
                                                     (get2DValue $ row column))))
          $)))

(defn smaller? [distances visited row col minVal]
  (cond
    (true? (get2DValue visited row col)) false
    (< (get2DValue distances row col) minVal) true
    :else false))

(defn getNextCoordinates [distances visited maxRowIdx maxColIdx]
  (loop [minRow nil
         minCol nil
         minVal Integer/MAX_VALUE
         row 0
         col 0]
    (if (> row maxRowIdx)
      [minRow minCol]
      (let [isSmaller (smaller? distances visited row col minVal)
            newMinRow (if (true? isSmaller) row minRow)
            newMinCol (if (true? isSmaller) col minCol)
            newMinVal (if (true? isSmaller) (get2DValue distances row col) minVal)]
        (recur newMinRow
               newMinCol
               newMinVal
               (if (= maxColIdx col) (inc row) row)
               (if (= maxColIdx col) 0 (inc col)))))))

(defn findDistances [input maxRowIdx maxColIdx]
  (loop [[row column] (list 0 0)
         distances (:distances input)
         visited (:visted input)]
    (if (true? (get2DValue visited maxRowIdx maxColIdx))
      distances
      (let [weights (:weigths input)
            updatedDistances (updateNeighboringDistances weights distances visited row column maxRowIdx maxColIdx)
            updatedVisited (update-in visited [row column] #(returnTrue %))
            nextCoordinates (getNextCoordinates updatedDistances updatedVisited maxRowIdx maxColIdx)]
        (recur nextCoordinates updatedDistances updatedVisited)))))

(defn extendOutFiveTimes [list]
  (loop [accum []
         current list
         itr 1]
    (if (> itr 5)
      (flatten accum)
      (let [next (map #(if (= % 9) 1 (inc %)) current)]
        (recur (conj accum current) next (inc itr))))))

(defn extendDownFiveTimes [arrayList]
  (loop [idx 0
         rows arrayList]
    (if (= idx (* (count arrayList) 4))
      rows
      (let [next (map #(if (= % 9) 1 (inc %)) (nth rows idx))]
        (recur (inc idx) (conj rows next))))))

(defn part-1
  "Day 15 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (hash-map :weigths $
                  :distances (vec (map #(vec (map returnMaxVal %)) $))
                  :visted (vec (map #(vec (map returnFalse %)) $)))
        (update-in $ [:distances 0 0] #(returnZero %))
        (update-in $ [:visted 0 0] #(returnTrue %))
        (findDistances $ (dec (count (:weigths $))) (dec (count (nth (:weigths $) 0))))
        (get2DValue $ (dec (count $)) (dec (count (nth $ 0))))))


(defn part-2
  "Day 15 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (map #(extendOutFiveTimes %) $)
        (vec $)
        (extendDownFiveTimes $)
        (hash-map :weigths $
                  :distances (vec (map #(vec (map returnMaxVal %)) $))
                  :visted (vec (map #(vec (map returnFalse %)) $)))
        (update-in $ [:distances 0 0] #(returnZero %))
        (update-in $ [:visted 0 0] #(returnTrue %))
        (findDistances $ (dec (count (:weigths $))) (dec (count (nth (:weigths $) 0))))
        (get2DValue $ (dec (count $)) (dec (count (nth $ 0))))))
