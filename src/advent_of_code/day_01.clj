(ns advent-of-code.day-01)

(defn- readNumbers [data]
  (->> data
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

(defn- calculateSlidingWindowDepths [depths slidingWindowDepths]
    (cond
      (< (count depths) 3) slidingWindowDepths
      :else (conj (calculateSlidingWindowDepths (rest depths) slidingWindowDepths)
                  (+ (first depths) (second depths) (nth depths 2)))))

(defn- slidingWindow [depths]
  (-> depths
      (calculateSlidingWindowDepths [])
      (reverse)))

(defn- countOfIncreases [depths]
    (cond
      (< (count depths) 2) 0
      (> (second depths) (first depths)) (+ 1 (countOfIncreases (rest depths)))
      :else (countOfIncreases (rest depths))))

(defn part-1
  "Day 01 Part 1"
  [input]
  (-> input
      (readNumbers)
      (countOfIncreases)))

(defn part-2
  "Day 01 Part 2"
  [input]
  (-> input
      (readNumbers)
      (slidingWindow)
      (countOfIncreases)))


