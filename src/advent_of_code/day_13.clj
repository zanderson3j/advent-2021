(ns advent-of-code.day-13
  (:require [clojure.string :as str]))

(defn- readNumbers [data]
  (->> data
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn- readFold [data]
  (as-> data $
       (str/split $ #"=")
       (list (nth $ 0) (Integer/parseInt (nth $ 1)))))

(defn- makeDottedArray [numberOfElements]
  (loop [array []
         element 0]
    (if (= numberOfElements element)
      array
      (recur (conj array ".") (+ element 1)))))

(defn- assembleInput [input]
  (loop [[line & lines] input
         isCoordinate true
         coordinates (list)
         folds (list)]
    (if (nil? line)
      (hash-map :coordinates coordinates :folds (reverse folds))
      (recur lines
             (if (and isCoordinate (not (str/blank? line))) true false)
             (if (and isCoordinate (not (str/blank? line)))
               (conj coordinates (readNumbers line)) coordinates)
             (if isCoordinate
               folds (conj folds (readFold line)))))))

(defn- makeBoard [input]
  (loop [width (->> (:coordinates input)
                    (map #(nth % 0))
                    (flatten)
                    (reduce max))
         height (->> (:coordinates input)
                     (map #(nth % 1))
                     (flatten)
                     (reduce max))
         board []]
    (if (< height 0)
      (assoc input :board board)
      (recur width (dec height) (conj board (makeDottedArray width))))))

(defn- hashTag [input] "#")

(defn- populateBoard [input]
  (loop [[coordinate & coordinates] (:coordinates input)
         board (:board input)]
    (if (nil? coordinate)
      (assoc input :board board)
      (recur coordinates (update-in board [(nth coordinate 1) (nth coordinate 0)] hashTag)))))

(defn- foldXCoordinates [coordinates split]
  (loop [[xy & xys] coordinates
         newCoordinates (list)]
    (if (nil? xy)
      newCoordinates
      (recur xys
             (if (>= (nth xy 0) split)
               (conj newCoordinates (list (- split (- (nth xy 0) split)) (nth xy 1)))
               (conj newCoordinates xy))))))

(defn- foldYCoordinates [coordinates split]
  (loop [[xy & xys] coordinates
         newCoordinates (list)]
    (if (nil? xy)
      newCoordinates
      (recur xys
             (if (>= (nth xy 1) split)
               (conj newCoordinates (list (nth xy 0) (- split (- (nth xy 1) split))))
               (conj newCoordinates xy))))))

(defn- getFoldFunction [fold]
  (if (= fold "fold along x") foldXCoordinates foldYCoordinates))

(defn- foldBoard [input]
  (loop [[fold & folds] (:folds input)
         board (:board input)
         coordinates (:coordinates input)]
    (if (nil? fold)
      board
      (let [foldFunction (getFoldFunction (nth fold 0))
            newCoordinates (foldFunction coordinates (nth fold 1))]
        (recur folds
               (:board (->> (hash-map :coordinates newCoordinates)
                            (makeBoard)
                            (populateBoard)))
               newCoordinates)))))

(defn part-1
  "Day 13 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (assembleInput $)
        (makeBoard $)
        (populateBoard $)
        (foldBoard (assoc $ :folds (take 1 (:folds $))))
        (flatten $)
        (filter #(= % "#") $)
        (count $)))

(defn part-2
  "Day 13 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (assembleInput $)
        (makeBoard $)
        (populateBoard $)
        (foldBoard $)
        ; replace the map function with comment for testing purposes
        ;(flatten $)
        ;(filter #(= % "#") $)
        ;(count $)
        (map #(println %) $)))
