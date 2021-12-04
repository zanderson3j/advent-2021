(ns advent-of-code.day-04
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn- readNumbersCalled [data]
  (let [numbersToCall (first data)]
    (as-> numbersToCall $
        (str/split $ #",")
        (map #(Integer/parseInt %) $))))

(defn- formatBoard [rows]
  (loop [board []
         remainingRows rows]
    (if (empty? remainingRows)
      board
      (let [row (first remainingRows)
            formattedRow (->> row
                              (re-seq #"-?\d+")
                              (map #(Integer/parseInt %)))
            builtRows (conj board formattedRow)]
        (recur builtRows (rest remainingRows))))))

(defn- readBoards [boardData]
  (loop [boards []
         remainingBoards boardData]
    (if (empty? remainingBoards)
      boards
      (let [nextBoard (take 5 remainingBoards)
            formattedBoard (formatBoard nextBoard)
            builtBoards (conj boards formattedBoard)]
        (recur builtBoards (drop (if (= 5 (count remainingBoards)) 5 6) remainingBoards))))))

(defn- columnsToRows [board]
  (loop [rowNumber 0
         flippedBoard []]
    (if (> rowNumber 4)
      flippedBoard
      (recur (+ rowNumber 1) (conj flippedBoard (map #(nth % rowNumber) board))))))

(defn- horizontalWin? [numbersCalled board]
  (loop [row (set (first board))
         rowsLeft (rest board)
         winner (set/subset? row (set numbersCalled))]
    (cond
      (empty? rowsLeft) {:winner false}
      (true? winner) (let [setsOfRows (map #(set %) board)
                           setOfRows (reduce set/union setsOfRows)
                           unmarkedSet (set/difference setOfRows numbersCalled)]
                       {:winner true
                        :unmarked unmarkedSet
                        :row row
                        :last-number-called (last numbersCalled)
                        :count-of-numbers-called (count numbersCalled)})
      :else (let [nextRow (first rowsLeft)
                  remainingRow (rest rowsLeft)
                  winner (set/subset? nextRow (set numbersCalled))]
              (recur nextRow remainingRow winner)))))

(defn- verticalWin? [numbersCalled board]
  (let [rowsFromColumns (columnsToRows board)]
    (horizontalWin? numbersCalled rowsFromColumns)))

(defn- winner? [numbersCalled board]
  (cond
    (true? (:winner (horizontalWin? numbersCalled board))) (assoc (horizontalWin? numbersCalled board) :board board)
    (true? (:winner (verticalWin? numbersCalled board))) (assoc (verticalWin? numbersCalled board) :board board)
    :else {:board board :winner false}))

(defn- getWinner [game]
  (let [numbersToCall (:numbersCalled game)
        boards (:boards game)]
    (loop [numbersCalling []
           numbersLeft numbersToCall
           scoredBoards {}]
      (if (some #(= (:winner %) true) scoredBoards)
        (nth (filter #(true? (:winner %)) scoredBoards) 0)
        (let [nextCall (conj numbersCalling (first numbersLeft))
              remainingNumbers (rest numbersLeft)
              scored (->> boards
                          (map #(winner? nextCall %)))]
          (recur nextCall remainingNumbers scored))))))

(defn- getLastToWin [game]
  (let [numbersToCall (:numbersCalled game)
        boards (:boards game)]
    (loop [numbersCalling []
           numbersLeft numbersToCall
           winningBoards []
           unscoredBoards boards]
      (if (empty? numbersLeft)
        winningBoards
        (let [nextCall (conj numbersCalling (first numbersLeft))
              remainingNumbers (rest numbersLeft)
              scores (->> unscoredBoards
                          (map #(winner? nextCall %)))
              winners (filter #(true? (:winner %)) scores)
              losers (filter #(false? (:winner %)) scores)]
          (recur nextCall
                 remainingNumbers
                 (if (empty? winners) winningBoards (conj winningBoards winners))
                 (map #(:board %) losers)))))))

(defn part-1
  "Day 04 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (hash-map :numbersCalled (readNumbersCalled $) :boards (readBoards (drop 2 $)))
        (getWinner $)
        (* (reduce + (:unmarked $)) (:last-number-called $))))

(defn part-2
  "Day 04 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (hash-map :numbersCalled (readNumbersCalled $) :boards (readBoards (drop 2 $)))
        (getLastToWin $)
        (map #(nth % 0) $)
        (sort-by :count-of-numbers-called $)
        (last $)
        (* (reduce + (:unmarked $)) (:last-number-called $))))
