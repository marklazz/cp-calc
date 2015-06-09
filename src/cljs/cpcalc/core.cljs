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
(def backtracking-result (atom nil))

(defn total-years-of-contribution []
  (reduce + 0 @durations))

(defn last-level-of-contribution [solution]
  (let [non-blank (filter #(> (last %) 0) (util/durations-with-indexes solution))
        reversed-non-blank (reverse non-blank)]
  (first (first reversed-non-blank))
  )
)

(defn durations-and-fees [solution]
  (vec (map vector solution config/fees)))

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

(defn value-with-currencies [amount]
  (str amount " (en dolares: " (/ amount @config/dollar-currency) ")")
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
         ])
       [:br]
       [:div
        [:span "Coeficiente de retorno mensual basado en ficto:"]
        [:input { :type "text" :value @util/roi-coeficient :on-change #(reset! util/roi-coeficient (-> % .-target .-value)) }]
       ]
       [:div
        [:span "Valor del dólar:"]
        [:input { :type "text" :value @config/dollar-currency :on-change #(reset! config/dollar-currency (-> % .-target .-value)) }]
       ]
       [:input { :type "button" :value "Calcular óptimo" :on-click #(reset! durations (backtracking/calculation)) }]
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
