(ns advent-of-code.day-07)

(defn- readNumbers [data]
  (->> data
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

(defn- incrementedFuelMove [finalPosition startingPosition]
  (let [numberOfSteps (Math/abs (int (- startingPosition finalPosition)))]
    (/ (* numberOfSteps (inc numberOfSteps)) 2)))

(defn- fuelUsageIncremented [positions maxPos]
  (loop [position maxPos
         fuelCounts []]
    (if (< position 0)
      fuelCounts
      (recur (dec position) (conj fuelCounts (->> positions
                                                  (map #(incrementedFuelMove position %))
                                                  (reduce +)))))))

(defn- fuelUsage [positions maxPos]
  (loop [position maxPos
         fuelCounts []]
    (if (< position 0)
      fuelCounts
      (recur (dec position) (conj fuelCounts (->> positions
                                                  (map #(Math/abs (int (- position %))))
                                                  (reduce +)))))))

(defn part-1
  "Day 07 Part 1"
  [input]
  (as-> input $
        (readNumbers $)
        (fuelUsage $ (reduce max $))
        (reduce min $)))

(defn part-2
  "Day 07 Part 2"
  [input]
  (as-> input $
        (readNumbers $)
        (fuelUsageIncremented $ (reduce max $))
        (reduce min $)))
