(ns cpcalc.config)

(def min-years-per-level 3)
(def initial-durations (vec (concat [1] (take 9 (repeat 3)) [2])))

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
(def dollar-currency (atom 26.5))
