(ns insane-noises.core
  (:use [overtone.live]
        [overtone.inst.piano]))

;;make a diverse "song" with different tones
(defn random [min max]
  (+ (rand-int (- max min)) min))

;(def foo [b] (synth (out 0 (pan2 (sin-osc b))))) ;just creates a synth

(defsynth my-sin [freq 440]
          (out 0 (pan2 (sin-osc freq)))) ;; like fn and defn, always need a value assigned or uses default

;;;Our ingredients
(def ingredients '((my-sin 200) (my-sin 300)
                   (my-sin 400) (my-sin 500) (my-sin 600) (my-sin 700) (my-sin (random 100 800)(random 1/16 1))))
;or just have random numbers
  ;randomly generated ~30 individuals

;creates a gene with freq + duration (actual length of notes)
(defn ram [] (vector (random 0 200)(take 1 (shuffle [1/2 1 2 4]))))

;;;;;;;

;;;;Error Function
(defn error [genome]
  (let [first-note (first (first genome))
        octave-up (+ 200 (first (last genome)))
        octave-down (- 200 (first (last genome)))
        unison (first (last genome))
        last-note (first (last genome))
        second-last-note (first (second (reverse genome)))]
    (if (or (= first-note octave-down) ;;octave
            (= first-note octave-up)
            (= first-note unison))
      200
      (if (= last-note (+ 125 second-last-note)) ;;tritone
        125
        1000))
  ))
;;nothing over a fifth -> +50
;;standard deviation of duration of notes + notes -> +100


;;;;;;;


(defn new-individual []
  "Returns a new, random individual in the context of test-pairs."
  (let [genome (vec (repeatedly 30 #(ram)))] ;the length is changeable 5 vs. ?
    {:genome genome
     :error  (error genome)}))

(defn best [individuals]
  "Returns the best of the given individuals."
  (reduce (fn [i1 i2]
            (if (< (:error i1) (:error i2))
              i1
              i2))
          individuals))

(defn pick [population]
  "Returns an individual selected from population using a tournament."
  (best (repeatedly 2 #(rand-nth population))))

(defn mutate [genome]
  "Returns a possibly-mutated copy of genome."
  (let [with-additions (flatten (for [g genome]
                                  (if (< (rand) 1/10)
                                    (shuffle (list g (rand-nth ingredients)))
                                    g)))
        with-deletions (flatten (for [g with-additions]
                                  (if (< (rand) 1/11)
                                    ()
                                    g)))]
    (vec with-deletions)))


(defn crossover [genome1 genome2]
  "Returns a one-point crossover product of genome1 and genome2."
  (let [crossover-point (rand-int (inc (min (count genome1)
                                            (count genome2))))]
    (vec (concat (take crossover-point genome1)
                 (drop crossover-point genome2)))))

(defn make-child [population test-pairs]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (mutate (crossover (:genome (pick population))
                                      (:genome (pick population))))]
    {:genome new-genome
     :error  (error new-genome)}))

(defn report [generation population]
  "Prints a report on the status of the population at the given generation."
  (let [current-best (best population)]
    (println {:generation   generation
              :best-error   (:error current-best)
              :diversity    (float (/ (count (distinct population))
                                      (count population)))
              :average-size (float (/ (->> population
                                           (map :genome)
                                           (map count)
                                           (reduce +))
                                      (count population)))
              :best-genome  (:genome current-best)})))

(defn gp [population-size generations test-pairs]
  "Runs genetic programming to solve, or approximately solve, a floating-point
  symbolic regression problem in the context of the given population-size,
  number of generations to run, and test-pairs."
  (loop [population (repeatedly population-size
                                #(new-individual test-pairs))
         generation 0]
    (report generation population)
    (println population)
    (if (or (< (:error (best population)) 0.1)
            (>= generation generations))
      (best population)
      (recur (repeatedly population-size
                         #(make-child population test-pairs))
             (inc generation)))))

;; A simple test, symbolic regression of x^2 + x + 1

(def simple-regression-data
  (for [x (range 100 800 50)]
    [(my-sin x)]))

#_(gp 10 10 simple-regression-data)

