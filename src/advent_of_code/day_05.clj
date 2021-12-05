(ns advent-of-code.day-05
  (:require [clojure.string :as str]))

(defn- readNumbers [data]
  (->> data
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn- makeZeroedArray [numberOfElements]
  (loop [list []
         element 0]
    (if (= numberOfElements element)
      list
      (recur (conj list 0) (+ element 1)))))

(defn updateHorizontalLine [grid line rowLength]
  (let [startOfRow (* (nth line 1) rowLength)
        startOfLine (min (nth line 0) (nth line 2))
        endOfLine (max (nth line 0) (nth line 2))
        startingElement (+ startOfRow startOfLine)
        lastElement (+ startOfRow endOfLine)]
    (loop [populatedGrid grid
           element startingElement]
      (if (< lastElement element)
        populatedGrid
        (recur (update populatedGrid element inc) (+ element 1))))))

(defn- updateVerticalLine [grid line rowLength]
  (let [column (nth line 0)
        startingRow (min (nth line 1) (nth line 3))
        lastRow (max (nth line 1) (nth line 3))]
    (loop [populatedGrid grid
           row startingRow]
      (if (< lastRow row)
        populatedGrid
        (recur (update populatedGrid (+ (* row rowLength) column) inc) (+ row 1))))))

(defn- updateDiagonalLine [grid line rowLength]
  (let [firstColumn (nth line 0)
        firstRow (nth line 1)
        lastColumn (nth line 2)
        lastRow (nth line 3)
        columnDelta (if (> firstColumn lastColumn) -1 1)
        rowDelta (if (> firstRow lastRow) -1 1)]
    (loop [populatedGrid grid
           xElement firstColumn
           yElement firstRow]
      (cond
        (and (neg? columnDelta) (> lastColumn xElement)) populatedGrid
        (and (pos? columnDelta) (< lastColumn xElement)) populatedGrid
        :else (recur (update populatedGrid (+ (* yElement rowLength) xElement) inc)
                     (+ xElement columnDelta)
                     (+ yElement rowDelta))))))

(defn- populateGrid [grid line rowLength]
  (cond
    (= (nth line 1) (nth line 3)) (updateHorizontalLine grid line rowLength)
    (= (nth line 0) (nth line 2)) (updateVerticalLine grid line rowLength)
    :else (updateDiagonalLine grid line rowLength)))

(defn- createVentMap [endpoints]
  (let [gridMaxElement (->> endpoints
                     (flatten)
                     (reduce max))
        grid (makeZeroedArray (* (+ gridMaxElement 1) (+ gridMaxElement 1)))]
    (loop [[line & lines] endpoints
           populatedGrid grid]
      (if (nil? line)
        populatedGrid
        (recur lines (populateGrid populatedGrid line (+ gridMaxElement 1)))))))

(defn part-1
  "Day 05 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (filter #(or (= (nth % 0) (nth % 2)) (= (nth % 1) (nth % 3))) $)
        (createVentMap $)
        (filter #(> % 1) $)
        (count $)))

(defn part-2
  "Day 05 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (filter #(or (= (nth % 0) (nth % 2))
                     (= (nth % 1) (nth % 3))
                     (and (= (abs (- (nth % 0) (nth % 2))) (abs (- (nth % 1) (nth % 3)))))) $)
        (createVentMap $)
        (filter #(> % 1) $)
        (count $)))
