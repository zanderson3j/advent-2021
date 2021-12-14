(ns advent-of-code.day-14
  (:require [clojure.string :as str]))

(defn- assembleInput [input]
  (let [polymer (char-array (nth input 0))
        ruleList (drop 2 input)]
    (loop [[rule & rules] ruleList
           ruleMap (hash-map)]
      (if (nil? rule)
        (hash-map :polymer polymer :rules ruleMap)
        (let [splitRule (str/split rule #" ")
              key (nth splitRule 0)
              value (nth splitRule 2)]
          (recur rules (assoc ruleMap key value)))))))

(defn- grow [polymer rules]
  (loop [[polymerOne & remainingPolymers] polymer
         mappedPolymers []]
    (if (nil? remainingPolymers)
      (if (nil? polymerOne) (vec (interleave polymer mappedPolymers))
                            (conj (vec (interleave polymer mappedPolymers)) polymerOne))
      (recur remainingPolymers
             (conj mappedPolymers (first (get rules (str polymerOne (nth remainingPolymers 0)))))))))

(defn- growPolymer [input iterations]
  (let [polymer (:polymer input)
        rules (:rules input)]
    (loop [polymerSequence polymer
           count (dec iterations)]
      (if (< count 0)
        polymerSequence
        (recur (grow polymerSequence rules) (dec count))))))

(defn- getStartingPairs [polymer]
  (loop [[element & remaining] polymer
         pairs []]
    (if (nil? remaining)
      pairs
      (recur remaining (conj pairs (str element (first remaining)))))))

(defn- makeCountMap [polymer]
  (loop [[element & remaining] polymer
         counts (hash-map)]
    (if (nil? element)
      counts
      (recur remaining
             (if (nil? (get counts element))
               (assoc counts element 1)
               (update counts element inc))))))

(defn- optomizedInput [input]
  (as-> input $
        (keys (:rules $))
        (map #(hash-map % 0) $)
        (reduce merge $)
        (assoc input :counts $)
        (assoc $ :startingPairs (getStartingPairs (:polymer $)))
        (assoc $ :polymer (makeCountMap (:polymer $)))))

(defn- startingPairCounts [input]
  (loop [[pair & pairs] (:startingPairs input)
         counts (:counts input)]
    (if (nil? pair)
      counts
      (recur pairs (update counts pair inc)))))

(defn- updateElementCounts [elementCounts pairCounts rules]
  (loop [[pairCount & remaining] (seq pairCounts)
         counts elementCounts]
    (if (nil? pairCount)
      counts
      (recur remaining
             (if (nil? (get elementCounts (first (get rules (nth pairCount 0)))))
               (assoc counts (first (get rules (nth pairCount 0))) (nth pairCount 1))
               (update counts (first (get rules (nth pairCount 0))) #(+ % (nth pairCount 1))))))))

(defn- updatePairCounts [pairCounts rules]
  (loop [[pairCount & remaining] (seq pairCounts)
         newPairCounts (->> rules
                            (keys)
                            (map #(hash-map % 0))
                            (reduce merge))]
    (if (nil? pairCount)
      newPairCounts
      (let [firstPair (str (first (nth pairCount 0)) (get rules (nth pairCount 0)))
            secondPair (str (get rules (nth pairCount 0)) (last (nth pairCount 0)))]
        (recur remaining (-> newPairCounts
                             (update firstPair #(+ % (nth pairCount 1)))
                             (update secondPair #(+ % (nth pairCount 1)))))))))

(defn- optomizedGrowPolymer [input iterations]
  (let [rules (:rules input)]
    (loop [elementCounts (:polymer input)
           pairCounts (startingPairCounts input)
           itr (dec iterations)]
      (if (< itr 0)
        elementCounts
        (recur (updateElementCounts elementCounts pairCounts rules)
               (updatePairCounts pairCounts rules)
               (dec itr))))))

(defn part-1
  "Day 14 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (assembleInput $)
        (growPolymer $ 10)
        (frequencies $)
        (sort-by val $)
        (- (nth (nth $ (dec (count $))) 1) (nth (nth $ 0) 1))))

(defn part-2
  "Day 14 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (assembleInput $)
        (optomizedInput $)
        (optomizedGrowPolymer $ 40)
        (sort-by val $)
        (- (nth (nth $ (dec (count $))) 1) (nth (nth $ 0) 1))))
