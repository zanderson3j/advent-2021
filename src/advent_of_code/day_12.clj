(ns advent-of-code.day-12
  (:require [clojure.string :as str]))

(defn- getAdjacentNodes [node graph]
  (->> graph
       (filter #(some #{node} %))
       (flatten)
       (filter #(not= "start" %))
       (filter #(not= node %))))

(defn- createNodeMap [nodes graph]
  (loop [[node & remainingNodes] nodes
         nodeMap (hash-map)]
    (if (nil? node)
      nodeMap
      (recur remainingNodes (assoc nodeMap node (getAdjacentNodes node graph))))))

(defn all-lowercase? [s]
  (= s (str/lower-case s)))

(defn- containsSmallDuplicate? [path]
  (let [smallCaves (->> path
                        (filter all-lowercase?))]
    (not (apply distinct? smallCaves))))

(defn- legalAddSingleSmallCave? [node path]
  (and (all-lowercase? node) (some #{node} path)))

(defn- legalAddDuplicateCave? [node path]
  (and (all-lowercase? node)
       (some #{node} path)
       (containsSmallDuplicate? path)))

(defn- pathToNewPaths [path newPaths nodeMap smallCaveRule]
  (loop [[node & nodes] (get nodeMap (first path))
         updatedNewPaths newPaths]
    (if (nil? node)
      updatedNewPaths
      (recur nodes (if (smallCaveRule node path)
                     updatedNewPaths
                     (conj updatedNewPaths (conj path node)))))))

(defn- takeStep [paths nodeMap smallCaveRule]
  (loop [[path & remainingPaths] (:inProgressPaths paths)
         newPaths (list)]
    (if (nil? path)
      newPaths
      (recur remainingPaths (pathToNewPaths path newPaths nodeMap smallCaveRule)))))

(defn- convertCompletedPaths [completedPaths inProgressPaths]
  (loop [[pathToAdd & remainingPaths] (filter #(= (first %) "end") inProgressPaths)
         nowComplete completedPaths]
    (if (nil? pathToAdd)
      (hash-map :completedPaths nowComplete
                :inProgressPaths (filter #(not= (first %) "end") inProgressPaths))
      (recur remainingPaths (conj nowComplete pathToAdd)))))

(defn- findPaths [graph smallCaveRule]
  (let [nodes (->> graph
                   (flatten)
                   (distinct))
        nodeMap (createNodeMap nodes graph)]
    (loop [paths (hash-map :completedPaths (list) :inProgressPaths (list (list "start")))]
      (if (and (> (count (:completedPaths paths)) 0)
               (empty? (:inProgressPaths paths)))
        paths
        (recur (convertCompletedPaths
                 (:completedPaths paths)
                 (takeStep paths nodeMap smallCaveRule)))))))

(defn part-1
  "Day 12 Part 1"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(str/split % #"-") $)
        (findPaths $ legalAddSingleSmallCave?)
        (count (:completedPaths $))))

(defn part-2
  "Day 12 Part 2"
  [input]
  (as-> input $
        (str/split-lines $)
        (map #(str/split % #"-") $)
        (findPaths $ legalAddDuplicateCave?)
        (count (:completedPaths $))))
