(ns advent-of-code.day-10
  (:require [clojure.string :as str]))

(defn- closingCharacterToScore [character]
  (cond (= character ")") 3
        (= character "}") 1197
        (= character "]") 57
        (= character ">") 25137
        :else nil))

(defn- closingCharacterToPoint [character]
  (cond (= character ")") 1
        (= character "}") 3
        (= character "]") 2
        (= character ">") 4
        :else nil))

(defn- closingCharacter? [character]
  (or (= character ")")
      (= character "}")
      (= character "]")
      (= character ">")))

(defn- openingCharacter? [character]
  (or (= character "(")
      (= character "{")
      (= character "[")
      (= character "<")))

(defn- closingToOpeningCharacter [closing]
  (cond (= closing ")") "("
        (= closing "}") "{"
        (= closing "]") "["
        (= closing ">") "<"
        :else nil))

(defn- openingToClosingCharacter [opening]
  (cond (= opening "(") ")"
        (= opening "{") "}"
        (= opening "[") "]"
        (= opening "<") ">"
        :else nil))

(defn- illegalChar? [character stack]
  (if (closingCharacter? character)
    (not= (closingToOpeningCharacter character) (first stack))
    false))

(defn- getFirstIllegalCharacter [list]
  (loop [[character & characters] list
         stack '()]
    (if (or (nil? character) (illegalChar? character stack))
      character
      (recur characters
             (if (openingCharacter? character)
               (conj stack character)
               (drop 1 stack))))))

(defn- legalLine? [list]
  (loop [[character & characters] list
         stack '()]
    (cond
      (nil? character) true
      (illegalChar? character stack) false
      :else (recur characters
                   (if (openingCharacter? character)
                     (conj stack character)
                     (drop 1 stack))))))

(defn- getUnmatchedCharacters [list]
  (loop [[character & characters] list
         stack '()]
    (if (nil? character)
      stack
      (recur characters
             (if (openingCharacter? character)
               (conj stack character)
               (drop 1 stack))))))

(defn part-1
  "Day 10 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(re-seq #"." %) $)
        (map #(getFirstIllegalCharacter %) $)
        (filter some? $)
        (map #(closingCharacterToScore %) $)
        (reduce + $)))

(defn part-2
  "Day 10 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(re-seq #"." %) $)
        (filter #(legalLine? %) $)
        (map #(getUnmatchedCharacters %) $)
        (map #(map (fn [character] (openingToClosingCharacter character)) %) $)
        (map #(map (fn [character] (closingCharacterToPoint character)) %) $)
        (map #(reduce (fn [accumulator value] (+ (* 5 accumulator) value)) 0 %) $)
        (sort $)
        (nth $ (/ (count $) 2))))
