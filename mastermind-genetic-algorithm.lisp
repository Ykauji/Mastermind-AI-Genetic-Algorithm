;; The Algorithm:

;; 1. Init population
;; 2. Set fitness for all individuals
;; 3. Selection algorithm to select "parents"
;; 4. Crossover algorithm to create "offspring"
;; 5. Assign fitness to new bambinos
;; 6. Repeat#3-5 until finished crossover.
;; 7. Mutate random dudes in the population using mutation algorithm
;; 8. Repeat until all mutation finished.
;; 9. Replacement algorithm to replace all old men with stronger babies. 
;; 10. Repeat this process until either goal is met, ie. time or fitness max. 
;; 11. Make the guess using the highest fitness in the population.
;; 12. Receive output and change constraints accordingly. 
;; 13. TRY AGAIN!

;; Works in progress: 
;; 1.) Complete
;; 2.) In progress
;; 3.) In progress, need 2.
;; 4.) In progress
;; 5.) In progress, need 2.
;; 7.) Complete 
;; 9.) In progress, need 2.
;; 12.) In progress
;; 13.)

;; Selection algorithm: Proportional to its fitness value. ie. higher fitness has high % of being selected. Thinking of using tournament selection.
;; Crossover algorithm: After selected parents, randomly change certain genes with a %. e.g crossover rate.
;; Mutation algorithm: Randomly mutate one gene randomly. 

(defclass mastermind-algorithm () 
	((fitness-calculator :initarg :fitness-calculator :initform (make-instance 'mastermind-fitness-calculator) 
		:accessor fitness-calculator :documentation "The calculator for the GA") 
	(population :initarg :population :initform (make-instance 'mastermind-population) 
		:accessor population :documentation "The current population!")  
	(time-per-guess :initarg :time-per-guess :initform 5.0 :accessor time-per-guess 
		:documentation "The time the GA has per guess") 
	(potential-colors-at-peg :initarg :potential-colors-at-peg :initform nil :accessor potential-colors-at-peg 
        :documentation "The potential colors at each peg") 
	(num-pegs :initarg :num-pegs :initform 4 :accessor num-pegs 
        :documentation "The number of pegs for this run.") 
	(num-colors :initarg :num-colors :initform 6 :accessor num-colors 
		:documentation "The number of colors for this run")
	(eligible-set :initarg :eligible-set :initform nil :accessor eligible-set 
		:documentation "The list of eligible set codes!") 
	(crossover :initarg :crossover :initform 0.5 :accessor crossover 
		:documentation "Chance the parents make bambinos") 
	(mutation-rate :initarg :mutation-rate :initform 0.03 :accessor mutation-rate 
		:documentation "Chance that individual becomes spiderman")
	(permutation-rate :initarg :permutation-rate :initform 0.03 :accessor permutation-rate 
		:documentation "Chance of permutation")
	(inversion-rate :initarg :inversion-rate :initform 0.02 :accessor inversion-rate 
		:documentation "Chance of inversion")
	(population-size :initarg :population-size :initform 150 :accessor population-size
		:documentation "The size of the population") 
	(eligible-set-size :initarg :eligible-set-size :initform 200 :accessor eligible-set-size 
		:documentation "The max size of the eligible set!")
	(tournament-set-size :initarg :tournament-set-size :initform 5 :accessor tournament-set-size 
		:documentation "The max size of the tournament set!")
	(max-gen-per-guess :initarg :max-gen-per-guess :initform 200 :accessor max-gen-per-guess 
		:documentation "Maximum allowed generations per guess")))

(defvar *Mastermind-Algorithm* (make-instance 'mastermind-algorithm :num-pegs 4 :num-colors 6))

;; Default test.

;; First run through the round. 
(defmethod mastermind-genetic-init ((self mastermind-algorithm) board colors) 
	(setf *Mastermind-Algorithm* (make-instance 'mastermind-algorithm :num-pegs board :num-colors colors 
		:population (make-instance 'mastermind-population) :fitness-calculator nil :population-size 150 :max-gen-per-guess 100))
	(setf (fitness-calculator *Mastermind-Algorithm*) (make-instance 'mastermind-fitness-calculator :num-positions board 
		:num-colors (num-colors *Mastermind-Algorithm*) :previous-guesses nil))
	(init-population (population *Mastermind-Algorithm*) (population-size *Mastermind-Algorithm*) (num-pegs *Mastermind-Algorithm*) (num-colors *Mastermind-Algorithm*)))

(defmethod mastermind-genetic-add-guess ((self mastermind-algorithm) guess) 
	(setf new-guess-and-score (list guess nil))
	(setf (previous-guesses (fitness-calculator self))
		(reverse (cons new-guess-and-score (reverse (previous-guesses (fitness-calculator self)))))))
; 4 1 2 3 
; 3 2 1 
;; '(A A B C) -> ( ((A A B C) (1 1 1)) ((A A C B) (1 1 2)) 
(defmethod mastermind-genetic-add-guess-score ((self mastermind-algorithm) response) 
	(setf (second (car (reverse (previous-guesses (fitness-calculator self))))) response))

(defun generate-initial-guess (pegs colors) 
		(return-from generate-initial-guess (choose-n-random pegs colors)))
		
;; Initial guess, scale for different values. Default 4,6.
(defun initial-guess (pegs colors) 
	(declare (ignore colors))
	;(setf (previous-guesses (fitness-calculator *Mastermind-Algorithm*)) (make-list 1))
	(setf (previous-guesses (fitness-calculator *Mastermind-Algorithm*)) (list (append '() (list (generate-initial-guess pegs (colors *Mastermind*)) nil)))))

(defmethod evolve-until-finished ((self mastermind-algorithm)) 
	(loop 
  	for new-population = (evolve-population self)
  	for current-generation from 1 to (max-gen-per-guess self)
  	until (>= (length (eligible-set self)) 
  		(eligible-set-size self))) 
	;; If eligible-set is still empty, then reset population and restart!
	(cond ((null (eligible-set self)) 
		(progn
		;(print "Population is still nil after evolving!") 
		;(setf (population-size self) (* (population-size self) 2))
		;(setf (max-gen-per-guess self) (/ (max-gen-per-guess self) 2))
		(init-population (population self) (population-size self) (num-pegs self) (num-colors self))
		(evolve-until-finished self)))))

;; Recursion or Loop?
(defmethod evolve-population ((self mastermind-algorithm)) 
;; Now we can calculate the heuristics of the population.
  (loop for individual in (individuals (population self)) do 
  	(setf (fitness individual) (calculate-fitness (fitness-calculator self) (color-values individual))))
  ;; Now start the evolution!
    (setf (individuals (population self)) (loop  
  	for new-child-number from 1 to (length (individuals (population self))) 
  	for is-now-a-father = (tournament-selection (population self) (tournament-set-size self)) 
  	for is-now-a-mother = (tournament-selection (population self) (tournament-set-size self))
  	for new-child = (make-instance 'mastermind-individual :color-values (1-point-crossover (population self) 
  		is-now-a-father is-now-a-mother) :num-pegs (num-pegs self) :colors (colors *Mastermind*) 
  	:fitness nil)
  	for mutated-baby = (mutate-individual-or-not new-child (mutation-rate self)) 
  	for inverted-baby = (to-invert-or-not-to-invert mutated-baby (inversion-rate self))
  	for permutated-baby = (to-permutate-or-not-to-permutate inverted-baby (permutation-rate self))
  	collect permutated-baby))
  	(loop for new-individual in (individuals (population self)) do
  		(if (not (null (is-eligible-individual (fitness-calculator self) (color-values new-individual)))) 
  			(setf (eligible-set self) (cons new-individual (eligible-set self))))))

(defun pathfinders-genetic-algorithm (board colors SCSA last-response)
  ;; If this is the first turn of the round!
  (if (not last-response) 
  		(progn 

  			(mastermind-genetic-init *Mastermind-Algorithm* board (number-of-colors *mastermind*)) 
  			(initial-guess board (number-of-colors *mastermind*))
  			;(mastermind-genetic-init *Mastermind-Algorithm* board colors)
  			;(initial-guess board colors)
  			;(return-from pathfinders-genetic-algorithm '(A A B C)))) HARDCODE STARTING VALUES!
  			(return-from pathfinders-genetic-algorithm (first (first( previous-guesses (fitness-calculator *Mastermind-Algorithm*)))))))
  ;; Else lets work with the last-response. format = (black,white,guess#)
  ;; First add the guess results to the last guess.
  ;(print "last response = ")
  ;(print last-response)
  (mastermind-genetic-add-guess-score *Mastermind-Algorithm* last-response) 
 
  ;; Recheck eligibility set. 
  (recheck-eligible-set *Mastermind-Algorithm*)
  ;(print (eligible-set *Mastermind-Algorithm*))
  ;; Evolve Population!
  (evolve-until-finished *Mastermind-Algorithm*)
  ;; Loop through everything in current population and add to list of eligible codes. 
  ;; If no eligible codes left, reset population.

  (setf best-guess (first (eligible-set *Mastermind-Algorithm*)))
  ;(progn 
  ;	(print "Guessing: ") 
  ;	(prin1 (1+ (third last-response))))
  ;(print (color-values best-guess))
  ;; Choose from population or evolve again.

  ;; Add guess to list, and then guess.
  (mastermind-genetic-add-guess *Mastermind-Algorithm* (color-values best-guess)) 
  (color-values best-guess))

(defmethod recheck-eligible-set ((self Mastermind-Algorithm)) 
	(setf (eligible-set self) (loop for individual in (eligible-set self)
		when (is-eligible-individual (fitness-calculator self) (color-values individual)) 
collect individual)))