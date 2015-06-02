(ns cpcalc.backtracking
    (:require [cpcalc.config :as config]
              [cpcalc.util :as util])
  )

;(defn valid-solutions []
  ;(filter is-valid-solution (all-solutions)))

;(defn is-valid-solution [solution]
  ;(let [last-index (last-level-of-contribution solution)
        ;subvector (subvec solution 2 last-index)]
  ;(and (= (first solution) 1)
       ;(= (get solution 1) 3)
       ;(every? #(and (> % 2) (= (mod % 3) 0)) subvector)
       ;(= (reduce + 0 solution) required-amount-of-years)
       ;)
  ;)
;)

;(defn next-solution [solution]
  ;(if (not (= solution config/initial-durations))
    ;(let [index-can-decrease (find-last-index-can-increase solution)]
      ;(if index-can-decrease (increase-duration2 solution index-can-decrease :down))
    ;)
  ;)
;)
;;(next-solution [1 3 3 22 0 0 0 0 0 0 0])

;(defn backtracking-calculation [current-solution better-solution roi]
  ;(let [next-sol (next-solution current-solution)
        ;next-sol-roi (roi-percentage next-sol)]
  ;(if next-sol
    ;(do
      ;(println (str "La solucion:" next-sol " next sol ROI:" next-sol-roi " current ROI:" roi " current sol:" current-solution))
      ;(if (> next-sol-roi roi)
        ;(backtracking-calculation next-sol next-sol next-sol-roi)
        ;(backtracking-calculation next-sol better-solution roi))
      ;)
      ;better-solution
    ;)
  ;)
;)
(def initial-backtracking-durations (vec (concat [1 3 26] (take 8 (repeat 0)))))

(defn can-decrease-duration-backtracking [solution i]
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
        next-level-accepts-years (> diff-years-accepted-next-level 0)]
    (do
      (and not-last-level greater-than-min-for-level
           can-move-min-number-of-years next-level-accepts-years)
    )
  )
)
;(apply concat [[[1 2]] [[3 4] [5 6]] [[7 8]]])

(defn decrease-duration-backtracking [solution i]
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

(defn backtracking-solutions [current-solution revised-solutions]
  (let [potential-movements (filter #(can-decrease-duration-backtracking current-solution %) (range (config/total-years)))
        next-solutions (map #(decrease-duration-backtracking current-solution %) potential-movements)
        new-solutions (filter #(not (contains? revised-solutions %)) next-solutions)
        new-revised (reduce (fn [res sol] (conj res sol)) revised-solutions new-solutions)
        roi (util/roi-percentage current-solution)]
    (do
      ;(println (str "ol revised:" revised-solutions "potential mov:" potential-movements " sols:" next-solutions " roi: " roi " new sols:" new-solutions " new reivsed:" new-revised))
      (println (str "ol revised:" revised-solutions "potential mov:" potential-movements " sols:" next-solutions " roi: " roi " new sols:" new-solutions " new reivsed:" new-revised))
      ;(println (str "ol revised:" revised-solutions "potential mov:" potential-movements " sols:" next-solutions " roi: " roi " new sols:" new-solutions " new reivsed:" new-revised))
      (if (empty? new-solutions)
        (do
          ;(println (str "la current sol:" current-solution " con roi:" roi))
          { :solution current-solution :roi roi }
        )
        (let [recursive-results (map #(backtracking-solutions % new-revised) new-solutions)]
            (first (reverse (sort-by :roi (concat recursive-results [{ :solution current-solution :roi roi }]))))
        )
      )
    )
  )
)
;(in-ns 'cpcalc.backtracking :reload)
;(backtracking-solutions initial-backtracking-durations #{})
;    (for [x potential-movements
         ;:let [current-duration-x (get @durations x)]]

  ;(if next-sol
    ;(do
      ;(println (str "La solucion:" next-sol " next sol ROI:" next-sol-roi " current ROI:" roi " current sol:" current-solution))
      ;(if (> next-sol-roi roi)
        ;(backtracking-calculation next-sol next-sol next-sol-roi)
        ;(backtracking-calculation next-sol better-solution roi))
      ;)
      ;better-solution
    ;)
  ;)
;)



;(can-decrease-duration-backtracking [1 3 18 6 3 0 0 0 0 0 0] 2)
;(map #(can-decrease-duration-backtracking [1 3 18 6 3 0 0 0 0 0 0] %) (range (config/total-years)))
;(filter #(can-decrease-duration-backtracking [1 3 15 6 6 0 0 0 0 0 0] %) (range (config/total-years)))
;(filter #(can-decrease-duration-backtracking [1 3 12 6 3 6 0 0 0 0 0] %) (range (config/total-years)))
;(map #(can-decrease-duration-backtracking [1 3 3 6 3 3 3 3 6 0 0] %) (range (config/total-years)))
;(map #(can-decrease-duration-backtracking [1 3 3 3 3 3 3 3 6 3 0] %) (range (config/total-years)))
;(map #(can-decrease-duration-backtracking [1 3 3 3 3 3 3 3 3 6 0] %) (range (config/total-years)))
;(filter #(can-decrease-duration-backtracking [1 3 3 3 3 3 3 3 3 3 2] %) (range (config/total-years)))
;(filter #(can-decrease-duration-backtracking [1 3 3 23 0 0 0 0 0 0 0] %) (range (config/total-years)))

;(in-ns 'cpcalc.backtracking :reload)
;(decrease-duration-backtracking [1 3 12 6 3 6 0 0 0 0 0] 2)
;(println (str "level i:" i " not-last-level:" not-last-level " greater-than-min-for-level:" greater-than-min-for-level " can-move-min-number-of-years:" can-move-min-number-of-years " next level accept years: " next-level-accepts-years))

;(can-decrease-duration-backtracking initial-backtracking-durations 3)
 ;(defn can-increase-duration-backtracking [solution i]
  ;(let [current-value (get solution i)
       ;max-value (:max (get fees i))
       ;can-decrease-lower-level (> (get solution (- i 1)) (:min (get fees (- i 1))))
       ;is-upper-level-than-last (= i (+ (last-level-of-contribution solution) 1))]
    ;(and (or (nil? max-value) (< current-value max-value))
         ;can-decrease-lower-level)
  ;)
;)
