;;;; Translation of Peter Norvig's sudoku solver to idiomatic Clojure
;;;; See http://norvig.com/sudoku.html
;;;;
;;;; Throughout this program we have:
;;;;   r is a row,     e.g. :a
;;;;   c is a column,  e.g. 3
;;;;   s is a square,  e.g. [:a 3]
;;;;   d is a digit,   e.g. 9
;;;;   u is a unit,    e.g. [[:a 1] [:b 1] [:c 1] ... [:i 1]]
;;;;   grid is a grid, e.g. 81 non-blank chars, e.g. starting with ".18...7..."
;;;;   values is a map of possible values, e.g. {[:a 1] #{1 2 3 9} [:a 2] #{8}}

(ns user
  (:use [clojure.string :only [join trim]]))



(def digits (set (range 1 10)))
(def rows [:a :b :c :d :e :f :g :h :i])
(def cols (range 1 10))
(def squares (for [r rows c cols] [r c]))
(def unitlist (concat (for [c cols] (for [r rows] [r c]))
                      (for [r rows] (for [c cols] [r c]))
                      (for [rs (partition 3 rows) cs (partition 3 cols)]
                        (for [r rs c cs] [r c]))))
(def units (into {} (for [s squares]
                      [s (for [u unitlist :when (some #{s} u)] u)])))
(def peers (into {} (for [s squares]
                      [s (-> (reduce into #{} (units s)) (disj s))])))

(declare reduce-true assign eliminate)

;;; Unit Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unit-tests
  "A set of tests that must pass"
  ;; NB: Tests normally go in separate files and and use clojure.test
  []
  (assert (= 81 (count squares)))
  (assert (= 27 (count unitlist)))
  (assert (every? #(= 3 (count (units %))) squares))
  (assert (every? #(= 20 (count (peers %))) squares))
  (assert (= (units [:c 2])
             [[[:a 2] [:b 2] [:c 2] [:d 2] [:e 2] [:f 2] [:g 2] [:h 2] [:i 2]]
              [[:c 1] [:c 2] [:c 3] [:c 4] [:c 5] [:c 6] [:c 7] [:c 8] [:c 9]]
              [[:a 1] [:a 2] [:a 3] [:b 1] [:b 2] [:b 3] [:c 1] [:c 2] [:c 3]]]))
  (assert (= (peers [:c 2])
             #{[:a 2] [:b 2] [:d 2] [:e 2] [:f 2] [:g 2] [:h 2] [:i 2]
               [:c 1] [:c 3] [:c 4] [:c 5] [:c 6] [:c 7] [:c 8] [:c 9]
               [:a 1] [:a 3] [:b 1] [:b 3]}))
  :passed)

;;; Parse a Grid ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn grid-values
  "Convert grid into a map of {square: digit}, with nil for empties"
  [grid]
  (zipmap squares (for [c grid :when (or (Character/isDigit c) (= \. c))]
                    (when-not (#{\0 \.} c)
                      (Character/digit c 10)))))

(defn parse-grid
  "Convert grid to a map of possible values, {square: digits}. Return false
  on contradiction"
  [grid]
  (reduce-true
   (fn [values [s d]] (assign values s d))
   (into {} (for [s squares] [s digits])) ;to start, any square can be any digit
   (remove (comp nil? val) (grid-values grid))))

;;; Constraint Propagation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn assign
  "Whittle down the square at s to digit d by eliminating every digit
  except d from the square, and doing constraint propogation. Returns
  false if a contradiction results"
  [values s d]
  (reduce-true #(eliminate %1 s %2)
               values
               (disj (values s) d)))

(defn eliminate
  "Eliminate digit d from square s and do any appropriate constraint
  propogation"
  [values s d]
  (if-not ((values s) d)
    values ;already eliminated
    (when-not (= #{d} (values s)) ;can't remove last value
      (let [values (update-in values [s] disj d)
            values (if (= 1 (count (values s)))
                     ;; Only one digit left, eliminate it from peers
                     (reduce-true #(eliminate %1 %2 (first (%1 s)))
                                  values
                                  (peers s))
                     values)]
        (reduce-true
         (fn [values u]
           (let [dplaces (for [s u :when ((values s) d)] s)]
             (when-not (zero? (count dplaces)) ;must be a place for this value
               (if (= 1 (count dplaces))
                 ;; Only one spot remaining for d in a unit -- assign it
                 (assign values (first dplaces) d)
                 values))))
         values
         (units s))))))

;;; Display as 2D Grid ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn display
  "Display values as a 2D grid"
  [values]
  (let [width (inc (apply max (map (comp count values) squares)))
        line (join \+ (repeat 3 (join (repeat (* 3 width) \-))))]
    (doseq [r rows]
      (println (join (for [c cols]
                       (format (str "%-" width "s%s")
                               (join (values [r c]))
                               (if (#{3 6} c) "|" "")))))
      (when (#{:c :f} r) (println line)))))

;;; Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn search
  "Using depth-first search and propagation, try all possible values"
  [values]
  (when values
    (let [scount (comp count values)] ;digits remaining
      (if (every? #(= 1 (scount %)) squares)
        values ;solved!
        (let [s (apply min-key scount (filter #(< 1 (scount %)) squares))]
          (some identity (for [d (values s)]
                           (search (assign values s d)))))))))

(defn solve [grid] (-> grid parse-grid search))

;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- reduce-true
  "Like reduce but short-circuits upon logical false"
  [f val coll]
  (when val
    (loop [val val, coll coll]
      (if (empty? coll)
        val
        (when-let [val* (f val (first coll))]
          (recur val* (rest coll)))))))

(defn sum [xs] (reduce + xs))

(defn transpose [xs] (apply map vector xs))

(defn from-file
  ([file] (from-file file "\n"))
  ([file sep] (-> file slurp trim (.split sep))))

;;; System Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solved?
  "A puzzle is solved if each unit is a permutation of the digits 1 to 9"
  [values]
  (and values (every? #(= (sort digits) (sort (mapcat values %))) unitlist)))

(defmacro time*
  "Evaluates expr and returns [value time-in-seconds]"
  [expr]
  `(let [start# (System/nanoTime)
         ret# ~expr]
     [ret# (/ (double (- (System/nanoTime) start#)) 1000000000.0)]))

(defn solve-all
  "Attempt to solve a sequence of grids. Report a summary of results."
  [grids name]
  (let [[results times] (transpose (map #(-> % solve solved? time*) grids))
        solved (count (filter true? results))
        n (count grids)]
    (when (< 1 n)
      (println
       (format
        "Solved %d of %d %s puzzles (avg %.2f secs (%.0f Hz), max %.2f secs)."
        solved n name (/ (sum times) n) (/ n (sum times)) (apply max times))))))

(defn random-puzzle
  "Make a random puzzle with N or more assignments. Restart on contradictions."
  ([] (random-puzzle 17))
  ([n]
     (let [done? (fn [values]
                   (let [ds (apply concat (filter #(= 1 (count %)) (vals values)))]
                     (and (<= n (count ds)) (<= 8 (count (distinct ds))))))
           steps (reductions #(assign %1 %2 (-> %2 %1 seq rand-nth))
                             (into {} (for [s squares] [s digits]))
                             (shuffle squares))
           values (first (filter #(or (not %) (done? %)) steps))]
       (if (nil? values)
         (recur n) ;contradiction - retry
         (join (for [ds (map values squares)]
                 (if (next ds) \. (first ds))))))))

(def grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")
(def grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")

(def data-dir "./")

;; Expected to be run at the REPL
(defn run []
  (unit-tests)
  (solve-all (from-file (str data-dir "easy50.txt")) "easy")
  (solve-all (from-file (str data-dir "top95.txt")) "hard")
  (solve-all (from-file (str data-dir "hardest.txt")) "hardest")
  (solve-all (repeatedly 99 random-puzzle) "random"))

(run)

