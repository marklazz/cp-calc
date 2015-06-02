(ns cpcalc.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [cpcalc.backtracking :as backtracking]
              [cpcalc.config :as config]
              [cpcalc.util :as util]
              [goog.history.EventType :as EventType])
    (:import goog.History))

;; -------------------------
;; Views
(def durations (atom config/initial-durations))
(def required-amount-of-years 30)
(def backtracking-result (atom nil))

(defn total-years-of-contribution []
  (reduce + 0 @durations))

(defn last-level-of-contribution [solution]
  (let [non-blank (filter #(> (last %) 0) (util/durations-with-indexes solution))
        reversed-non-blank (reverse non-blank)]
  (first (first reversed-non-blank))
  )
)

;(defn levels-greater-than-min [solution]
  ;)
(def default-step 3)

(defn durations-and-fees [solution]
  (vec (map vector solution config/fees)))

(defn lower-levels-with-duration-and-config [solution i]
  (subvec (durations-and-fees solution) 0 i))

(defn upper-levels-with-duration-and-config [solution i]
  (subvec (durations-and-fees solution) i))

(defn can-move-years-in-other-level [[duration level]]
  (>= (- duration default-step) (:min level))
)

(defn can-increase-upper-duration [solution i]
  (let [current-value (get solution i)
       max-value (:max (get config/fees i))
       can-decrease-lower-level (can-move-years-in-other-level (get (durations-and-fees solution) i))
       is-last (= (last-level-of-contribution solution) i)
       ]
    (do ;(println "loco: " (last-level-of-contribution solution))
      (and (or (nil? max-value) (< current-value max-value))
           can-decrease-lower-level is-last))
  )
  )
;(map #(can-increase-upper-duration [1 3 14 6 6 0 0 0 0 0 0] %) (range (total-years)))
;(map vector [1 3 14 6 6 0 0 0 0 0 0] fees)
;(map #(> (- (first %) default-step) (:min (last %))) (lower-levels-with-duration-and-config solution i))
;(some can-move-years-in-other-level (lower-levels-with-duration-and-config solution i))
;can-move-years-in-other-level (get (lower-levels-with-duration-and-config solution i) 4)


(defn can-increase-lower-duration [solution i]
 (let [current-value (get solution i)
       max-value (:max (get config/fees i))
       can-decrease-upper-level (some can-move-years-in-other-level (subvec (vec (map vector solution config/fees)) i))
       can-decrease-lower-level (some can-move-years-in-other-level (subvec (vec (map vector solution config/fees)) 0 i))
       upper-level-positive (> (get @durations (+ i 1)) 0)
       ]
      (and (or (nil? max-value) (< current-value max-value))
           can-decrease-upper-level upper-level-positive)
  )
)

(defn increase-duration-from-upper [solution i]
  (let [last-level (last-level-of-contribution solution)
        decreased-durations (assoc solution last-level (- (get solution last-level) 1))]
    (assoc decreased-durations i (+ (get decreased-durations i) 1))
  )
)

(defn increase-duration-from-lower [solution i]
  (let [decreased-durations (assoc solution (- i 1) (- (get solution (- i 1)) 1))]
    (assoc decreased-durations i (+ (get decreased-durations i) 1))
  )
)

(defn can-increase-duration [i]
  (let [current-value (get @durations i)
       max-value (:max (get config/fees i))
       can-decrease-upper-level (> (get @durations (+ i 1)) 0)
       can-decrease-lower-level (> (get @durations (- i 1)) (:min (get config/fees (- i 1))))
       is-upper-level-than-last (= i (+ (last-level-of-contribution @durations) 1))]
  (and (or (nil? max-value) (< current-value max-value))
       (or can-decrease-upper-level can-decrease-lower-level))
  )
)

(defn increase-duration [state i]
  (let [can-decrease-upper-level (> (get @durations (+ i 1)) 0)
        can-decrease-lower-level (> (get @durations (- i 1)) (:min (get config/fees (- i 1))))
        is-last-level (= i (- (count state) 1))]
    (do
      (if (can-increase-duration i)
        (if can-decrease-upper-level
          (let [last-level (last-level-of-contribution @durations)
                decreased-durations (assoc state last-level (- (get state last-level) 1))]
            (do
              (assoc decreased-durations i (+ (get decreased-durations i) 1))
              )
          )
          (if can-decrease-lower-level
            (let [decreased-durations (assoc state (- i 1) (- (get state (- i 1)) 1))]
              (do
                (println (str "los decreased " decreased-durations))
                (assoc decreased-durations i (+ (get decreased-durations i) 1))
                )
              )
            (assoc state i (+ (get state i) 1))
          )
        )
        state
      )
    )
    )
  )

(defn increase-duration2 [state i direction]
  (if (= direction :up)
    (increase-duration-from-upper state i)
    (increase-duration-from-lower state i)
    )
  )

(defn can-decrease-duration [i]
  (let [current-value (get @durations i)
        min-for-level (:min (get config/fees i))]
  (and (or (> current-value min-for-level) (= (last-level-of-contribution @durations) i))
       (> current-value 0) (can-increase-duration (- i 1)))
  )
)
(defn decrease-duration [state i]
  (if (can-decrease-duration i)
    (let [decreased-durations (assoc state i (- (get state i) 1))]
      (assoc decreased-durations (- i 1) (+ (get decreased-durations (- i 1)) 1))
    )
    state
  )
)

;(defn can-decrease-duration2 [solution i]
  ;(let [current-value (get solution i)
        ;min-for-level (:min (get fees i))]
      ;(and (or (> current-value min-for-level) (= (last-level-of-contribution solution) i))
           ;(> current-value 0) (can-increase-duration2 solution (- i 1) :up))
      ;)
;)

;(defn decrease-duration2 [state i]
  ;(if (can-decrease-duration2 state i)
    ;(let [decreased-durations (assoc state i (- (get state i) 1))]
      ;(assoc decreased-durations (- i 1) (+ (get decreased-durations (- i 1)) 1))
    ;)
    ;state
  ;)
;)

(def dollar-currency (atom 26.5))
(defn value-with-currencies [amount]
  (str amount " (en dolares: " (/ amount @dollar-currency) ")")
)

(defn years-to-equal-investment []
  (let [estimated-last-salary-average (util/average-salary-last-three-years @durations)
        monthly-return (util/roi estimated-last-salary-average)
        total-expenses (util/expenses @durations)
        annual-return (* 12 monthly-return)]
    (/ total-expenses annual-return)
  )
)

(defn result []
  (let [estimated-last-salary-average (util/average-salary-last-three-years @durations)
        monthly-return (util/roi estimated-last-salary-average)]
     [:div.result
        [:div (str "Promedio salario en últimos 3 años: " estimated-last-salary-average)]
        [:div (str "Gastos: " (value-with-currencies (util/expenses @durations)))]
        [:div (str "Ganancia mensual esperada: " (value-with-currencies monthly-return))]
        [:div (str "Años para desquitar inversión: " (years-to-equal-investment))]
        [:div (str "Total años de contribución: " (total-years-of-contribution))]
        [:div (str "ROI (%): " (util/roi-percentage @durations))]
    ]
  )
)

;(is-valid-solution [1 2 3 4 5 6 7 8 9 10 11])
;(is-valid-solution [1 3 3 4 3 3 3 3 3])
;(is-valid-solution [1 3 3 3 3 3 3 3 3 3 2])
;(is-valid-solution [1 3 3 1 3 3 3 3 3 3 2])
;(is-valid-solution [1 2 3 4 5 6 7 8 9 0 0])
;(is-valid-solution [1 2 3 4 5 6 7 0 9 0 0])
;(is-valid-solution [1 3 2 24 0 0 0 0 0 0 0])
;(is-valid-solution [1 3 26 0 0 0 0 0 0 0 0])
;(last-level-of-contribution [1 3 23 0 0 0 0 0 0 0 0])
;(subvec [1 2 26 0 0 0 0 0 0 0 0] 1 2)
;(subvec [1 2 3 4 5 6 7 8 9 10 11] 1 10)

;(defn all-solutions []
  ;(for [
        ;years-2nd-level (range 3 (total-years))
        ;years-3rd-level (range (total-years))
        ;years-4th-level (range (total-years))
        ;years-5th-level (range (total-years))
        ;years-6th-level (range (total-years))
        ;years-7th-level (range (total-years))
        ;years-8th-level (range (total-years))
        ;years-8th-level (range (total-years))
        ;years-9th-level (range (total-years))
        ;years-10th-level (range (total-years))
        ;]
    ;(concat [1] [years-2nd-level] [years-3rd-level] [years-4th-level] [years-5th-level]
      ;[years-6th-level] [years-7th-level] [years-8th-level] [years-9th-level] [years-10th-level])
  ;))

(defn home-page []
  (let [deref-durations @durations]
  [:div.app
   [:h3 "Calculadora para optimización de inversión en Caja de Profesionales"]
   [:br]
   [:div
     [:form
       (for [x (range (config/total-years))
             :let [current-duration-x (get @durations x)]]
         ^{:key x}
         [:div
           [:span (str "En Escalón " x " durante " current-duration-x " años")]
           [:input { :type "button" :value "Más años en escalón (+)" :on-click #(swap! durations increase-duration x) :style { :display (if (can-increase-duration x) "" "none") } }]
           [:input { :type "button" :value "Menos años en escalón (-)" :on-click #(swap! durations decrease-duration x) :style { :display (if (can-decrease-duration x) "" "none") } }]
           ;[:input { :type "button" :value "Más años en escalón, tomado al escalón superior (+)" :on-click #(swap! durations increase-duration x :up) :style { :display (if (can-increase-duration2 @durations x :up) "" "none") } }]
           ;[:input { :type "button" :value "Más años en escalón, tomado al escalón inferior (+)" :on-click #(swap! durations increase-duration x :down) :style { :display (if (can-increase-duration2 @durations x :down) "" "none") } }]
           ;[:input { :type "button" :value "Menos años en escalón (-)" :on-click #(swap! durations decrease-duration2 x) :style { :display (if (can-decrease-duration2 @durations x) "" "none") } }]
         ])
       [:br]
       [:div
        [:span "Coeficiente de retorno mensual basado en ficto:"]
        [:input { :type "text" :value @util/roi-coeficient :on-change #(reset! @util/roi-coeficient (-> % .-target .-value)) }]
       ]
       [:div
        [:span "Valor del dólar:"]
        [:input { :type "text" :value @dollar-currency :on-change #(reset! dollar-currency (-> % .-target .-value)) }]
       ]
       [:input { :type "button" :value "Calcular óptimo" :on-click #(reset! durations (:solution (backtracking/backtracking-solutions backtracking/initial-backtracking-durations backtracking/initial-backtracking-durations #{}))) }]
       ;[:span "Resultado optimo:" :style { :display (if @backtracking-result "" "none") }]
     ]
     [result]
   ]
   [:div { :style { :clear "both" } }]
   [:div [:a {:href "#/about"} "Acerca de..."]]
  ]
  )
)

(defn about-page []
  [:div
   [:h2 "Desarrollado por Marcelo Giorgi"]
   [:div [:a {:href "#/"} "Ir a la pagina principal"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-root))
