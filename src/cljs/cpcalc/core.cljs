(ns cpcalc.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType])
    (:import goog.History))

;; -------------------------
;; Views

(def durations (atom (vec (concat [1] (take 9 (repeat 3)) [2]))))

(def fees [
  { :fee 1274 :salary 15211 :min 3 :max 1 }
  { :fee 2529 :salary 15211 :min 3 :max 3 }
  { :fee 4783 :salary 28772 :min 3 } ;2nd
  { :fee 6779 :salary 40775 :min 3 } ;3rd
  { :fee 8503 :salary 51147 :min 3 }
  { :fee 9957 :salary 59888 :min 3 }
  { :fee 11153 :salary 67087 :min 3 }
  { :fee 12089 :salary 72718 :min 3 }
  { :fee 12750 :salary 76692 :min 3 }
  { :fee 13151 :salary 79104 :min 3 }
  { :fee 13280 :salary 79876 :min 3 :max 2 }
])

(defn total-years [] (count fees))

(def roi-coeficient (atom 0.5))

(defn roi [amount] (* @roi-coeficient amount))

(def required-amount-of-years 30)

(defn total-years-of-contribution []
  (reduce + 0 @durations))
(defn durations-with-indexes []
  (map vector (range (total-years)) @durations))


(defn last-level-of-contribution []
  (let [non-blank (filter #(> (last %) 0) (durations-with-indexes))
        reversed-non-blank (reverse non-blank)]
  (first (first reversed-non-blank))
  )
)

(defn can-increase-duration [i]
  (let [current-value (get @durations i)
       max-value (:max (get fees i))
       can-decrease-upper-level (> (get @durations (+ i 1)) 0)
       can-decrease-lower-level (> (get @durations (- i 1)) (:min (get fees (- i 1))))
       is-upper-level-than-last (= i (+ (last-level-of-contribution) 1))]
  (and (or (nil? max-value) (< current-value max-value))
       (or can-decrease-upper-level can-decrease-lower-level))
  )
)

(defn increase-duration [state i]
  (let [can-decrease-upper-level (> (get @durations (+ i 1)) 0)
        can-decrease-lower-level (> (get @durations (- i 1)) (:min (get fees (- i 1))))
        is-last-level (= i (- (count state) 1))]
    (do
      (if (can-increase-duration i)
        (if can-decrease-upper-level
          (let [last-level (last-level-of-contribution)
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
        min-for-level (:min (get fees i))]
  (and (or (> current-value min-for-level) (= (last-level-of-contribution) i))
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

(defn average-salary-last-three-years []
  (let [levels-in-last-years (levels-during-last-years [] @durations)
        salary-last-years (map (fn [level] (:salary (get fees (- level 1)))) levels-in-last-years)
        average (/ (reduce + salary-last-years) 3)]
    average
  )
)

(defn expenses []
  (reduce (fn [res [index duration]] (+ res (* 12 (* duration (:fee (get fees index)))))) 0 (durations-with-indexes))
)

(def dollar-currency (atom 26.5))
(defn value-with-currencies [amount]
  (str amount " (en dolares: " (/ amount @dollar-currency) ")")
)

(defn years-to-equal-investment []
  (let [estimated-last-salary-average (average-salary-last-three-years)
        monthly-return (roi estimated-last-salary-average)
        total-expenses (expenses)
        annual-return (* 12 monthly-return)]
    (/ total-expenses annual-return)
  )
)

(defn result []
  (let [estimated-last-salary-average (average-salary-last-three-years)
        monthly-return (roi estimated-last-salary-average)
        roi-percentage (* (/ monthly-return (expenses)) 100)]
     [:div.result
        [:div (str "Promedio salario en últimos 3 años: " estimated-last-salary-average)]
        [:div (str "Gastos: " (value-with-currencies (expenses)))]
        [:div (str "Ganancia mensual esperada: " (value-with-currencies monthly-return))]
        [:div (str "Años para desquitar inversión: " (years-to-equal-investment))]
        [:div (str "Total años de contribución: " (total-years-of-contribution))]
        [:div (str "ROI (%): " roi-percentage)]
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
       (for [x (range (total-years))
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
        [:input { :type "text" :value @roi-coeficient :on-change #(reset! roi-coeficient (-> % .-target .-value)) }]
       ]
       [:div
        [:span "Valor del dólar:"]
        [:input { :type "text" :value @dollar-currency :on-change #(reset! dollar-currency (-> % .-target .-value)) }]
       ]
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
