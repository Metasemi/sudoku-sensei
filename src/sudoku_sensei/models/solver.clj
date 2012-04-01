;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SudokuSensei: A Sudoku solver that also teaches.
;;;
;;; Given a partially-filled Sudoku puzzle, this module has
;;; several functions, some of which are pedagogical, and some of
;;; which are infrastructure for the pedagogy:
;;;
;;; - It can find any number of solutions to the puzzle
;;; (limited, of course, by the number of possible solutions).
;;; This capability underlies all the others.
;;;
;;; - It can indicate those squares that have only one or two
;;; possible entries (given the current state).  This is used to
;;; guide the solver either to squares they may have missed in
;;; their first pass or those they should consider for use of
;;; Ariadne's thread.
;;;
;;; - Given a square that has multiple possible entries, it can
;;; indicate which of those entries lead to a solution.  This is
;;; used to teach the use of Ariadne's thread by allowing the
;;; solver either to guess right or wrong and see how things
;;; proceed from there.
;;; 

(ns sudoku-sensei.models.solver
  [:require clojure.set])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Data Structures and Algorithms
;;;
;;; There are three projections on every Sudoku puzzle: rows,
;;; columns, and quadrants.  Each puzzle square is in one row,
;;; column, and quadrant.  The possible values in a blank square
;;; are constrained by the existing values in the projections
;;; which contain that square; in particular it can only have a
;;; value that doesn't already exist in any of the projections.
;;;
;;; This constraint relationship between squares and projections
;;; means that puzzles can be solved via constraint relaxation:
;;; when you inject a value into a square, you relax the network
;;; to find the values forced by that injection.  During the
;;; relaxation, you may get a conflict, in which case the
;;; injected value can't lead to a solution.  I suspect (although
;;; I haven't proved) that the converse is also true: if a given
;;; injected value does *not* lead to a conflict, then there is a
;;; solution which contains that value.
;;;
;;; When you inject a value into a square, that value propagates
;;; along the projections that contain that square, then along
;;; the projections that contain other squares in those
;;; projections, and so on.  Because of the high connectivity of
;;; the projections (each row connects with all the columns, and
;;; vice versa), any change has ripple effects which can rebound
;;; and interfere with each other.
;;; 
;;; Because of the constraint-based relationship between squares,
;;; and the concurrency of ripple effects, it's natural to model
;;; squares as agents which are observed by their containing
;;; rows, columns, and quadrants.  This serves to
;;; transactionally control changes to each square while ensuring
;;; that the effects of all changes are considered.

(defn row [puzzle index]
  "row of index in puzzle"
  (quot index (puzzle :sidelen)))

(defn col [puzzle index]
  "col of index in puzzle"
  (rem index (puzzle :sidelen)))

(defn address [puzzle row col]
  "address of square <row, col> in puzzle"
  (+ (* row (puzzle :sidelen)) col))

(defn value
  "value of square in puzzle"
  ([puzzle index]
     (nth (puzzle :vals) index))
  ([puzzle row col]
     (value puzzle (address puzzle row col))))

(defn rowvals
  "set of values in a square's row in puzzle"
  ([puzzle index]
     (let [row (row puzzle index)]
       (set (for [col (range (puzzle :sidelen))
                  :let [val (value puzzle row col)]
                  :when (not= val 0)]
              val))))
  ([puzzle row col]
     (rowvals puzzle (address puzzle row col))))

(defn colvals
  "set of values in a square's column in puzzle"
  ([puzzle index]
     (let [col (col puzzle index)]
       (set (for [row (range (puzzle :sidelen))
                  :let [val (value puzzle row col)]
                  :when (not= val 0)]
              val))))
  ([puzzle row col]
     (colvals puzzle (address row col puzzle))))

(defn quad
  "quadrant of index in puzzle"
  ([puzzle index]
     [(quot (row puzzle index) (puzzle :quadlen))
      (quot (col puzzle index) (puzzle :quadlen))])
  ([puzzle row vol]
     [(quot row (puzzle :quadlen))
      (quot col (puzzle :quadlen))]))

(defn quadvals
  "set of values in a square's quadrant in puzzle"
  ([puzzle index]
     (let [[quadrow quadcol] (quad puzzle index)
           quadlen (puzzle :quadlen)
           baserow (* quadrow quadlen)
           basecol (* quadcol quadlen)]
       (set (for [row (range baserow (+ baserow quadlen))
                  col (range basecol (+ basecol quadlen))
                  :let [val (value puzzle row col)]
                  :when (not= val 0)]
              val))))
  ([puzzle row col]
     (quadvals puzzle (address puzzle row col))))

(defn possible-values
  ([puzzle index]
     "Find the possible vaues for a given square.
These are all the values not already taken by other
squares in its row, column, or quadrant."
     (let [rowvals (rowvals puzzle index)
           colvals (colvals puzzle index)
           quadvals (quadvals puzzle index)
           allvals (set (range 1 (+ 1 (puzzle :sidelen))))]
       (clojure.set/difference allvals rowvals colvals quadvals)))
  ([puzzle]
     "Find all the possible values for each square in the puzzle."
     (vec (for [index (range (puzzle :size))]
            (if (= 0 (value puzzle index))
                (possible-values puzzle index)
              #{})))))

(defn unique-in-quad? [puzzle index guess]
  "Find whether guess is a possible value anywhere other than index in its quad."
  (let [{:keys [quadlen possibles]} puzzle
        [quadrow quadcol] (quad puzzle index)
        baserow (* quadrow quadlen)
        basecol (* quadcol quadlen)]
    (not-any? #(some #{guess} (nth possibles %))
              (for [row (range baserow (+ baserow quadlen))
                    col (range basecol (+ basecol quadlen))
                    :let [square (address puzzle row col)]
                    :when (not= square index)]
                square))))

(defn final-guesses [puzzle]
  "compute the row/col/quad possible values on a square or puzzle.
First we just look at the possible values for each square.
Then we factor in squares in quads with no overlap in guesses."
  (let [possibles (puzzle :possibles)]
    (vec
     (for [index (range (puzzle :size))
           :let [guesses (nth possibles index)]]
       (if (<= (count guesses) 1)
           guesses
         (letfn [(must-be [guess]
                   (and (unique-in-quad? puzzle index guess) #{guess}))]
           (or (some must-be guesses) guesses)))))))

(defn analyze [puzzle]
  "Compute guesses based on the values in a puzzle."
  (let [puzzle (assoc puzzle :possibles (possible-values puzzle))]
    (assoc puzzle :guesses (final-guesses puzzle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; I/O

(defn to-puzzle [seq]
  "Convert a sequence of values to a puzzle."
  (if (map? seq)
    seq
    (let [size (count seq)
          [sidelen quadlen] (cond (= size 81) [9 3]
                                  (= size 256) [16 4]
                                  :else (throw (IllegalArgumentException.
                                                "Sequence is not 9x9 or 16x16")))
          puzzle (hash-map :sidelen sidelen :quadlen quadlen :size size :vals (vec seq))]
      (analyze puzzle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; solver actions

;;; The sensei works on one puzzle at a time.
;;; The state of solving the puzzle is in a map:
;;; :initial - initial state puzzle
;;; :current - current state of puzzle
;;; :steps - the stack of solution steps, each a list:
;;;     (prior-puzzle solver-step-applied args-to-solver-step-applied)
(def solver-state {})

(defn current-state []
  "Get the current puzzle state."
  (let [puzzle (get solver-state :current)]
    (and puzzle
         (hash-map :puzzle puzzle
                   :step-count (count (get solver-state :steps))
                   :solved? (not-any? #{0} (puzzle :vals))
                   :auto-choice? (some #(= 1 (count %)) (puzzle :guesses)) 
                   :unsolvable? (or (some #{-1} (puzzle :vals))
                                    (some #(and (= 0 (first %)) (= 0 (count (second %))))
                                          (map #'list (puzzle :vals) (puzzle :guesses)))
                                    (not-any? #(> (count %) 0) (puzzle :guesses)))))))

(defn prepare-to-solve [puzzle]
  "Prepare to start solving the given puzzle."
  (alter-var-root #'solver-state
                  #(assoc %1 :initial puzzle :current puzzle :steps ())))

(defn restart-solution []
  (prepare-to-solve (get solver-state :initial)))

(defn try-solution-step [fn & args]
  "Try a solution step on the current state"
  (apply #'alter-var-root #'solver-state fn args))

(defn undo-solution-step []
  "Back out the last solution step."
  (alter-var-root #'solver-state #(let [state %
                                        steps (get state :steps)]
                                    (if (seq steps)
                                        (assoc state
                                          :current (ffirst steps)
                                          :steps (rest steps))
                                      state))))

(defn relax1 [state]
  "single-step relax the current state constraint network"
  (let [puzzle (state :current)
        steps (state :steps)
        {:keys [size vals guesses]} puzzle
        newvals (vec (for [index (range size)
                           :let [val (nth vals index)
                                 guess-set (nth guesses index)
                                 count (count guess-set)]]
                       (if (not= 0 val)
                           val
                         (cond (= 0 count) -1
                               (= 1 count) (first guess-set)
                               :else val))))
        newpuz (assoc puzzle :vals newvals)]
    (assoc state
      :current (analyze newpuz)
      :steps (cons (list puzzle #'relax1) steps))))

(defn relax [state]
  "fully relax the constraint network in puzzle"
  (let [puzzle (state :current)
        vals (puzzle :vals)
        newstate (relax1 state)
        newpuz (newstate :current)
        newvals (newpuz :vals)]
    (if (or (= vals newvals) (some #{-1} newvals))
        state
      (recur newstate))))

(defn inject1 [state index val]
  "inject the given value into a square in the current state"
  (let [puzzle (state :current)
        steps (state :steps)
        vals (nth (puzzle :guesses) index)
        oldval (nth (puzzle :vals) index)
        newval (cond (not= 0 oldval) oldval
                     (some #{val} vals) val
                     (= val :first) (first (nth (puzzle :guesses) index))
                     (= val :second) (second (nth (puzzle :guesses) index))
                     (= val :third) (nth (nth (puzzle :guesses) index) 3)
                     :else (throw (IllegalArgumentException.
                                   (str val " is not a valid entry for square <"
                                        (row puzzle index) ", "
                                        (col puzzle index) ">"))))
        newvals (assoc (puzzle :vals) index newval)
        newpuz (assoc puzzle :vals newvals)]
    (if (= newval oldval)
        state
      (assoc state
        :current (analyze newpuz)
        :steps (cons (list puzzle #'inject1 index val) steps)))))

(defn inject [state type]
  "inject a suitable value into puzzle"
  (let [puzzle (state :current)
        index2 (first (for [index (range (puzzle :size))
                            :let [vals (nth (puzzle :guesses) index)]
                            :when (= (count vals) 2)]
                        index))
        indexN (first (for [index (range (puzzle :size))
                            :let [vals (nth (puzzle :guesses) index)]
                            :when (> (count vals) 2)]
                        index))
        index (or index2 indexN)]
    (cond (= type :first) (inject1 puzzle index :first)
          (= type :second) (inject1 puzzle index :second)
          (= type :third) (if index2
                              (inject1 puzzle indexN :first)
                            (inject1 puzzle indexN :third))
          (and index2 (= type :fourth)) (inject1 puzzle indexN :second)
          (and index2 (= type :fifth)) (inject1 puzzle indexN :third)
          :else (throw (IllegalArgumentException.
                        (str "There are not enough injection points for " type "."))))))

