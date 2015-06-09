(ns cpcalc.backtracking
    (:require [cpcalc.config :as config]
              [cpcalc.util :as util])
  )

(def initial-durations (vec (concat [1 3 26] (take 8 (repeat 0)))))

(defn can-decrease-duration [solution i]
  (let [current-value (get solution i)
        min-for-level (:min (get config/fees i))
        not-last-level (< i (config/total-years))
        can-move-min-number-of-years (>= current-value (* config/min-years-per-level 2))
        greater-than-min-for-level (> current-value min-for-level)
        next-index (+ i 1)
        next-level-value (get solution next-index)
        max-years-next-level (:max (get config/fees next-index))
        max-years-can-move (min config/min-years-per-level (- current-value config/min-years-per-level))
        diff-years-accepted-next-level (if (nil? max-years-next-level) max-years-can-move (min max-years-next-level max-years-can-move))
        next-level-accepts-years (> diff-years-accepted-next-level 0)]
    (do
      (and not-last-level greater-than-min-for-level
           can-move-min-number-of-years next-level-accepts-years)
    )
  )
)

(defn decrease-duration [solution i]
  (let [current-value (get solution i)
        min-for-level (:min (get config/fees i))
        not-last-level (< i 10)
        can-move-min-number-of-years (>= current-value (* config/min-years-per-level 2))
        greater-than-min-for-level (> current-value min-for-level)
        next-index (+ i 1)
        next-level-value (get solution next-index)
        max-years-next-level (:max (get config/fees next-index))
        max-years-can-move (min config/min-years-per-level (- current-value config/min-years-per-level))
        diff-years-accepted-next-level (if (nil? max-years-next-level) max-years-can-move (min max-years-next-level max-years-can-move))
        next-level-accepts-years (> diff-years-accepted-next-level 0)
        decreased-durations (assoc solution i (- (get solution i) diff-years-accepted-next-level))]
    (assoc decreased-durations next-index (+ (get decreased-durations next-index) diff-years-accepted-next-level))
  )
)

(def revised-solutions (atom #{}))

(defn algorithm [current-solution]
  (let [potential-movements (filter #(can-decrease-duration current-solution %) (range (config/total-years)))
        next-solutions (map #(decrease-duration current-solution %) potential-movements)
        current-revised @revised-solutions
        new-solutions (filter #(not (contains? current-revised %)) next-solutions)
        roi (util/roi-percentage current-solution)]
    (do
      (if (empty? new-solutions)
        (do
          { :solution current-solution :roi roi }
        )
        (let [recursive-results (map algorithm new-solutions)]
          (do
            (doall (map #(swap! revised-solutions conj %) new-solutions))
            (first (reverse (sort-by :roi (conj recursive-results { :solution current-solution :roi roi }))))
          )
        )
      )
    )
  )
)

(defn calculation []
  (do
    (reset! revised-solutions #{})
    (:solution (algorithm initial-durations))
  )
)
