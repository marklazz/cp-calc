(ns cpcalc.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType])
    (:import goog.History))

;; -------------------------
;; Views

(def durations (atom (vec (take 10 (repeat 3)))))

(def fees [
  { :fee 1274 :salary 15211 :min 3 :max 3 }
  { :fee 2529 :salary 15211 :min 3 }
  { :fee 4783 :salary 28772 :min 3 } ;2a
  { :fee 6779 :salary 40775 :min 3 } ;3a
  { :fee 8503 :salary 51147 :min 3 }
  { :fee 11153 :salary 67087 :min 3 }
  { :fee 12089 :salary 72718 :min 3 }
  { :fee 12750 :salary 76692 :min 3 }
  { :fee 13151 :salary 79104 :min 3 }
  { :fee 13280 :salary 79876 :min 3 }
])

(def roi-coeficient (atom 0.5))

(defn roi [amount] (* @roi-coeficient amount))

(defn increase-duration [state i]
  (assoc state i (+ (get state i) 1))
)
(defn decrease-duration [state i]
  (assoc state i (- (get state i) 1))
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
  (let [duration-with-index (map vector (range 10) @durations)]
  (reduce (fn [res [index duration]] (+ res (* 12 (* duration (:fee (get fees index)))))) 0 duration-with-index)
  )
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
        monthly-return (roi estimated-last-salary-average)]
     [:div.result
        [:div (str "Promedio salario en ultimos 3 años: " estimated-last-salary-average)]
        [:div (str "Gastos: " (value-with-currencies (expenses)))]
        [:div (str "Ganancia mensual esperada: " (value-with-currencies monthly-return))]
        [:div (str "Años para desquitar inversion: " (years-to-equal-investment))]
    ]
  )
)

(defn home-page []
  (let [deref-durations @durations]
  [:div.app
   [:h1 "Calculadora para optimizacion de inversion en Caja de Profesionales"]
   [:br]
   [:form
     (for [x (range 10)
           :let [current-duration-x (get @durations x)]]
       ^{:key x}
       [:div
         [:span (str "Año: " x " duracion:" current-duration-x)]
         [:input { :type "button" :value "+" :on-click #(swap! durations increase-duration x) }]
         [:input { :type "button" :value "-" :on-click #(swap! durations decrease-duration x) }]
       ])
     [:br]
     [:div
      [:span "Coeficiente de retorno mensual basado en ficto:"]
      [:input { :type "text" :value @roi-coeficient :on-change #(reset! roi-coeficient (-> % .-target .-value)) }]
     ]
     [:div
      [:span "Valor del dolar:"]
      [:input { :type "text" :value @dollar-currency :on-change #(reset! dollar-currency (-> % .-target .-value)) }]
     ]
   ]
   [:br]
   [result]
   [:div [:a {:href "#/about"} "go to about page"]]
  ]
  )
)

(defn about-page []
  [:div [:h2 "About cpcalc"]
   [:div [:a {:href "#/"} "go to the home page"]]])

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
