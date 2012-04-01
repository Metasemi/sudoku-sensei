(ns sudoku-sensei.views.welcome
  (:require [sudoku-sensei.views.common :as common]
            [sudoku-sensei.models.solver :as solver]
            [sudoku-sensei.models.samples :as samples])
  (:use [noir.core :only [defpage]]
        [hiccup.core :only [html]]))

(defpage "/" []
  (common/layout
   [:h1 "Welcome to the Sudoku Sensei"]
   [:p "Choose a puzzle:"]
   [:table {:border 0 :rules 0 :style "empty-cells:show;"}
    [:tr {:align "center"}
     [:td (common/layout-puzzle samples/one-star :size 20 :hinting "none")]
     [:td "&nbsp;&nbsp;"]
     [:td (common/layout-puzzle samples/three-star :size 20 :hinting "none")]
     [:td "&nbsp;&nbsp;"]
     [:td (common/layout-puzzle samples/five-star :size 20 :hinting "none")]]
    [:tr {:align "center"}
     [:td [:a {:href "/solve?op=start&pname=one-star"} "Easy"]]
     [:td]
     [:td [:a {:href "/solve?op=start&pname=three-star"} "Medium"]
     [:td]
     [:td [:a {:href "/solve?op=start&pname=five-star"} "Hard"]]]]
    [:tr [:td {:style "height=30px" :colspan 5}
          [:hr {:style "vertical-align=middle"}]]]
    [:tr {:align "center"}
     [:td (common/layout-puzzle samples/chron-11-06-08 :size 20 :hinting "none")]
     [:td]
     [:td (common/layout-puzzle samples/chron-11-06-09 :size 20 :hinting "none")]]
    [:tr {:align "center"}
     [:td [:a {:href "/solve?op=start&pname=chron-08"} "Chronicle June 8, 2011"]]
     [:td]
     [:td [:a {:href "/solve?op=start&pname=chron-09"} "Chronicle June 9, 2011"]]]]))

(defpage "/solve" {:keys [op pname index guess]}
  (case op
    "start" (solver/prepare-to-solve (case pname
                                       "one-star" samples/one-star
                                       "three-star" samples/three-star
                                       "five-star" samples/five-star
                                       "chron-08" samples/chron-11-06-08
                                       "chron-09" samples/chron-11-06-09))
    "inject" (solver/try-solution-step #'solver/inject1
                                       (Integer/parseInt index)
                                       (Integer/parseInt guess))
    "back" (solver/undo-solution-step)
    "relax1" (solver/try-solution-step #'solver/relax1)
    "relax" (solver/try-solution-step #'solver/relax)
    "restart" (solver/restart-solution)
    "hint-none" (common/hint-none)
    "hint-one" (common/hint-one)
    "hint-two" (common/hint-two)
    "hint-all" (common/hint-all)
    (common/hint-all))
  (let [{:keys [puzzle solved? unsolvable? auto-choice? step-count]}
        (solver/current-state)]
    (common/layout
     [:h1 "Sudoku Sensei - Solving..."]
     [:p (str "Puzzle at step " step-count " (hinting = " common/hinting "):")]
     (common/layout-puzzle puzzle)
     [:p
      (if (= step-count 0)
          "At start"
        [:a {:href "/solve?op=back"} "Back one step"])
      " &mdash; "
      (cond solved? "Puzzle is solved!"
            unsolvable? "Puzzle can't be solved!"
            (not auto-choice?) "Manual guess required"
            :else [:a {:href "/solve?op=relax1"} "Auto-solve one step"])
      " &mdash; "
      (cond (or solved? unsolvable?) [:a {:href "/solve?op=restart"}
                                      "Restart from beginning"]
            (not auto-choice?) [:a {:href "/solve?op=hint-two"} "Show choices"]
            :else [:a {:href "/solve?op=relax"} "Auto-solve until choice needed"])]
     [:p
      [:a {:href "/solve?op=hint-none"} "Don't show hints"] " &mdash; "
      [:a {:href "/solve?op=hint-one"} "Hint 1-value squares"] " &mdash; "
      [:a {:href "/solve?op=hint-two"} "Hint 2-value squares"] " &mdash; "
      [:a {:href "/solve?op=hint-all"} "Hint all squares"]]
     [:p 
      [:a {:href "/"} "Choose new puzzle"]]
     )))
