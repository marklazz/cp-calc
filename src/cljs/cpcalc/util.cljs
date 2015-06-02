(ns cpcalc.util
    (:require [reagent.core :as reagent :refer [atom]]
              [cpcalc.config :as config])
  )

(def roi-coeficient-number 0.5)

(def roi-coeficient (atom roi-coeficient-number))

(defn roi [amount] (* @roi-coeficient amount))

(defn levels-during-last-years [result durations]
  (let [reverse-durations (reverse durations)
        level-count (count durations)
        last-duration (first reverse-durations)
        count-result (count result)
        reversed-list-except-last-level (rest reverse-durations)
        list-except-last-level (reverse reversed-list-except-last-level)
        first-element (first reverse-durations)
        list-decreased-on-last-year (reverse (concat [(- first-element 1)] reversed-list-except-last-level))]
    (if (= count-result 3)
      result
      (if (> last-duration 0)
        (if (> count-result 0)
          (levels-during-last-years (concat [level-count] result) list-decreased-on-last-year)
          (levels-during-last-years [level-count] list-decreased-on-last-year)
        )
        (levels-during-last-years result list-except-last-level)
      )
    )
  )
)

(defn average-salary-last-three-years [solution]
  (let [levels-in-last-years (levels-during-last-years [] solution)
        salary-last-years (map (fn [level] (:salary (get config/fees (- level 1)))) levels-in-last-years)
        average (/ (reduce + salary-last-years) 3)]
    average
  )
)
(defn durations-with-indexes [solution]
  (map vector (range (config/total-years)) solution))

(defn expenses [solution]
  (reduce (fn [res [index duration]] (+ res (* 12 (* duration (:fee (get config/fees index)))))) 0 (durations-with-indexes solution))
)

(defn roi-percentage [solution]
  (let [estimated-last-salary-average (average-salary-last-three-years solution)
        monthly-return (roi estimated-last-salary-average)]
        (* (/ monthly-return (expenses solution)) 100)
  )
)
