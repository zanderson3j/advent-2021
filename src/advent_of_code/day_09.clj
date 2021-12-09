(ns advent-of-code.day-09
  (:require [clojure.string :as str]))

(defn- readNumbers [data]
  (->> data
       (re-seq #"\d")
       (map #(Integer/parseInt %))))

(defn- addIfLowest [number adjacents lowPoints row index]
  (if (< number (reduce min adjacents))
    (conj lowPoints number)
    lowPoints))

(defn- addElementsIfLowest [number adjacents lowPoints row index]
  (if (< number (reduce min adjacents))
    (conj lowPoints (list row index))
    lowPoints))

(defn- getLowPoints [heightMap lowPointFunction]
  (let [lastRow (dec (count heightMap))
        lastIndex (dec (count (nth heightMap 0)))]
    (loop [row 0
           index 0
           lowPoints []]
      (if (> row lastRow)
        lowPoints
        (let [point (nth (nth heightMap row) index)]
          (recur (if (= index lastIndex) (inc row) row)
                 (if (= index lastIndex) 0 (inc index))
                 (cond
                   (and (= row 0) (= index 0)) (lowPointFunction point
                                                                 (list (nth (nth heightMap row) (inc index))
                                                                       (nth (nth heightMap (inc row)) index))
                                                                 lowPoints row index)
                   (and (= row 0) (= index lastIndex)) (lowPointFunction point
                                                                         (list (nth (nth heightMap row) (dec index))
                                                                               (nth (nth heightMap (inc row)) index))
                                                                         lowPoints row index)
                   (and (= row 0) (not= index 0) (not= index lastIndex)) (lowPointFunction point
                                                                                           (list (nth (nth heightMap row) (inc index))
                                                                                                 (nth (nth heightMap row) (dec index))
                                                                                                 (nth (nth heightMap (inc row)) index))
                                                                                           lowPoints row index)
                   (and (= row lastRow) (= index 0)) (lowPointFunction point
                                                                       (list (nth (nth heightMap row) (inc index))
                                                                             (nth (nth heightMap (dec row)) index))
                                                                       lowPoints row index)
                   (and (= row lastRow) (= index lastIndex)) (lowPointFunction point
                                                                               (list (nth (nth heightMap row) (dec index))
                                                                                     (nth (nth heightMap (dec row)) index))
                                                                               lowPoints row index)
                   (and (= row lastRow) (not= index 0) (not= index lastIndex)) (lowPointFunction point
                                                                                                 (list (nth (nth heightMap row) (inc index))
                                                                                                       (nth (nth heightMap row) (dec index))
                                                                                                       (nth (nth heightMap (dec row)) index))
                                                                                                 lowPoints row index)
                   (= index 0) (lowPointFunction point
                                                 (list (nth (nth heightMap row) (inc index))
                                                       (nth (nth heightMap (inc row)) index)
                                                       (nth (nth heightMap (dec row)) index))
                                                 lowPoints row index)
                   (= index lastIndex) (lowPointFunction point
                                                         (list (nth (nth heightMap row) (dec index))
                                                               (nth (nth heightMap (inc row)) index)
                                                               (nth (nth heightMap (dec row)) index))
                                                         lowPoints row index)
                   :else (lowPointFunction point
                                           (list (nth (nth heightMap row) (inc index))
                                                 (nth (nth heightMap row) (dec index))
                                                 (nth (nth heightMap (inc row)) index)
                                                 (nth (nth heightMap (dec row)) index))
                                           lowPoints row index))))))))

(defn- walkLeft [row index heightMap basin remainingPoints]
  (loop [stillToSearch (if (nil? remainingPoints) '() remainingPoints)
         element (dec index)]
    (if (or (< element 0)
            (= (nth (nth heightMap row) element) 9)
            (< (nth (nth heightMap row) element) (nth (nth heightMap row) (inc element))))
      stillToSearch
      (recur (if (> (nth (nth heightMap row) element) (nth (nth heightMap row) (inc element)))
               (if (or (some #(= (list row element) %) stillToSearch)
                       (some #(= (list row element) %) basin))
                 stillToSearch
                 (conj stillToSearch (list row element)))
               stillToSearch)
             (dec element)))))

(defn- walkRight [row index heightMap basin remainingPoints]
  (loop [stillToSearch (if (nil? remainingPoints) '() remainingPoints)
         element (inc index)]
    (if (or (> element (dec (count (nth heightMap row))))
            (= (nth (nth heightMap row) element) 9)
            (< (nth (nth heightMap row) element) (nth (nth heightMap row) (dec element))))
      stillToSearch
      (recur (if (> (nth (nth heightMap row) element) (nth (nth heightMap row) (dec element)))
               (if (or (some #(= (list row element) %) stillToSearch)
                       (some #(= (list row element) %) basin))
                 stillToSearch
                 (conj stillToSearch (list row element)))
               stillToSearch)
             (inc element)))))

(defn- walkUp [row index heightMap basin remainingPoints]
  (loop [stillToSearch (if (nil? remainingPoints) '() remainingPoints)
         element (dec row)]
    (if (or (< element 0)
            (= (nth (nth heightMap element) index) 9)
            (< (nth (nth heightMap element) index) (nth (nth heightMap (inc element)) index)))
      stillToSearch
      (recur (if (> (nth (nth heightMap element) index) (nth (nth heightMap (inc element)) index))
               (if (or (some #(= (list element index) %) stillToSearch)
                       (some #(= (list element index) %) basin))
                 stillToSearch
                 (conj stillToSearch (list element index)))
               stillToSearch)
             (dec element)))))

(defn- walkDown [row index heightMap basin remainingPoints]
  (loop [stillToSearch (if (nil? remainingPoints) '() remainingPoints)
         element (inc row)]
    (if (or (> element (dec (count heightMap)))
            (= (nth (nth heightMap element) index) 9)
            (< (nth (nth heightMap element) index) (nth (nth heightMap (dec element)) index)))
      stillToSearch
      (recur (if (> (nth (nth heightMap element) index) (nth (nth heightMap (dec element)) index))
               (if (or (some #(= (list element index) %) stillToSearch)
                       (some #(= (list element index) %) basin))
                 stillToSearch
                 (conj stillToSearch (list element index)))
               stillToSearch)
             (inc element)))))

(defn- basinCoordinates [heightMap lowPoint]
  (loop [basin (list)
         [point & remainingPoints] (list lowPoint)]
    (if (nil? point)
      basin
      (let [row (nth point 0)
            index (nth point 1)]
        (recur (conj basin point) (->> remainingPoints
                                        (walkLeft row index heightMap basin)
                                        (walkRight row index heightMap basin)
                                        (walkUp row index heightMap basin)
                                        (walkDown row index heightMap basin)))))))

(defn part-1
  "Day 09 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (getLowPoints $ addIfLowest)
        (map inc $)
        (reduce + $)))

(defn part-2
  "Day 09 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(readNumbers %) $)
        (hash-map :heightMap $ :lowPoints (getLowPoints $ addElementsIfLowest))
        (map #(basinCoordinates (:heightMap $) %) (:lowPoints $))
        (map count $)
        (sort $)
        (reverse $)
        (take 3 $)
        (reduce * $)))
