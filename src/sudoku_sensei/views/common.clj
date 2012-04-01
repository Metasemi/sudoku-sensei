(ns sudoku-sensei.views.common
  (:require [sudoku-sensei.models.solver :as solver])
  (:use [noir.core :only [defpartial]]
        [hiccup.page-helpers :only [include-css html5]]))

(defpartial layout [& content]
            (html5
              [:head
               [:title "Sudoku Sensei"]
               (include-css "/css/puzzle.css")]
              [:body
               [:div#wrapper
                content]]))

;;; Hinting state for puzzle display
(def hinting "none")

(defn hint-none []
  "Don't hint the puzzle display."
  (alter-var-root #'hinting #(or "none" %)))

(defn hint-one []
  "Only hint the single-value squares."
  (alter-var-root #'hinting #(or "one" %)))

(defn hint-two []
  "Only hint the one- or two-value squares."
  (alter-var-root #'hinting #(or "two" %)))

(defn hint-all []
  "Show all guesses for all squares."
  (alter-var-root #'hinting #(or "all" %)))

(defpartial layout-puzzle [puzzle & keyvals]
  (let [{:keys [sidelen quadlen vals guesses]} puzzle
        options (apply #'hash-map keyvals)
        hinting (get options :hinting hinting)
        cell-size (get options :size 40)]
    (letfn [(top-border [row] (if (= 0 (rem row quadlen)) "3px;" "1px;"))
            (bottom-border [row] (if (= 0 (rem (+ 1 row) quadlen)) "3px;" "1px;"))
            (left-border [col] (if (= 0 (rem col quadlen)) "3px;" "1px;"))
            (right-border [col] (if (= 0 (rem (+ 1 col) quadlen)) "3px;" "1px;"))
            (halign [guess-count] (if (<= 1 guess-count) "right;" "center;"))
            (valign [guess-count] (if (<= 1 guess-count) "top;" "middle;"))
            (size [guess-count] (if (<= 1 guess-count) "60%;" "100%;"))
            (inject [index guess]
              [:a {:href (str "/solve?op=inject&index=" index "&guess=" guess)}
               (str guess)])]
      (apply #'vector
             :table.sudoku {:frame "box"
                            :width (* sidelen (+ (if (= hinting "all") 30 5) cell-size))}
             (for [row (range sidelen)]
               (apply #'vector
                      :tr {:align "center"}
                      (for [col (range sidelen)
                            :let [index (solver/address puzzle row col)
                                  val (vals index)
                                  guess-set (nth guesses index)
                                  guess-count (count guess-set)]]
                        [:td.sudoku
                         {:style (str "height:" cell-size "px;"
                                      "padding:2px;" 
                                      "border-top-width:" (top-border row)
                                      "border-bottom-width:" (bottom-border row)
                                      "border-left-width:" (left-border col)
                                      "border-right-width:" (right-border col)
                                      "text-align:" (halign guess-count)
                                      "vertical-align:" (valign guess-count)
                                      "font-size:" (size guess-count))}
                         (cond (> val 0) val
                               (< val 0) [:font {:color "red"} "&#x220E;"]
                               (= guess-count 0) [:font {:color "red"} "&#x220E;"]
                               (= hinting "none") " "
                               (= guess-count 1) (inject index (first guess-set))
                               (= hinting "one") " "
                               (= guess-count 2) (mapcat #(list (inject index %) " ")
                                                         guess-set)
                               (= hinting "two") " "
                               (<= guess-count 6) (mapcat #(list (inject index %) " ")
                                                          guess-set)
                               :else " ")])))))))