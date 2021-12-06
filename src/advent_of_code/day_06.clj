(ns advent-of-code.day-06)

(defn- readNumbers [data]
  (->> data
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

(defn- makeZeroedArray [numberOfElements]
  (loop [list []
         element 0]
    (if (= numberOfElements element)
      list
      (recur (conj list 0) (+ element 1)))))

;(defn- spawn [lanternFish]
;  (loop [numberToSpawn (count (filter #(= % -1) lanternFish))
;         populatedSchool lanternFish]
;    (if (= numberToSpawn 0)
;      populatedSchool
;      (recur (dec numberToSpawn) (conj populatedSchool 8)))))

;(defn- passTheDay [lanternFish]
;  (->> lanternFish
;       (map #(dec %))
;       (spawn)
;       (map #(if (neg? %) 6 %))))

;(defn- runLanternFishSimulation [startingFish days]
;  (loop [daysLeft days
;         lanternFish (list startingFish)]
;    (println daysLeft)
;    (if (= daysLeft 0)
;      lanternFish
;      (recur (dec daysLeft) (passTheDay lanternFish)))))

(defn- spawnParentsOffspring [counts startingDay]
  (loop [index startingDay
         countsOfOffspring counts]
    (if (> index (dec (count counts)))
      countsOfOffspring
      (recur (+ index 7) (update countsOfOffspring index inc)))))

(defn- spawnOffspringsOffspring [counts startingDay]
  (loop [index startingDay
         numberSpawning (nth counts (- startingDay 9))
         countsOfOffspring counts]
    (if (> index (dec (count counts)))
      countsOfOffspring
      (recur (+ index 7) numberSpawning (update countsOfOffspring index + numberSpawning)))))

(defn- getOffspring [startingFish days]
  (loop [day 9
         counts (as-> days $
                      (inc $)
                      (makeZeroedArray $)
                      (spawnParentsOffspring $ (inc startingFish)))]
    (if (> day (dec (count counts)))
      counts
      (recur (inc day) (spawnOffspringsOffspring counts day)))))

(defn part-1
  "Day 06 Part 1"
  [input]
  (as-> input $
        (readNumbers $)
        (map #(getOffspring % 80) $)
        (+ (count $) (reduce + (flatten $)))))

;(map #(runLanternFishSimulation % 80) $)
;(flatten $)
;(count $)

(defn part-2
  "Day 06 Part 2"
  [input]
  (as-> input $
        (readNumbers $)
        (map #(getOffspring % 256) $)
        (+ (count $) (reduce + (flatten $)))))
