;; class for the population
(defclass mastermind-population ()
	((individuals :initarg individuals :initform nil :accessor individuals :documentation "Population of GA")))

(defvar *Mastermind-Population* (make-instance 'mastermind-population))

;; inits a random population
(defmethod init-population ((self mastermind-population) population-size num-pegs num-colors) 
	(setf (individuals self) 
		(make-list population-size :initial-element nil))
	(setf (individuals self) (loop for bambino from 1 to (length (individuals self)) 
		collect (create-new-individual num-pegs num-colors))))

;; This doesn't work right now! Fix sort by lambda later.
(defmethod sort-population-by-fitness ((self mastermind-population)) 
	(sort (individuals self) #'compare-individuals-lesser))

;; Individuals are mastermind-individual classes
(defun compare-individuals-greater (individual individual2) 
	(if (> (fitness individual) (fitness individual2)) individual individual2))

(defun compare-individuals-lesser (individual individual2) 
	(if (< (fitness individual) (fitness individual2)) individual individual2))

(defmethod get-individual ((self mastermind-population) index)
	(nth index (individuals self)))

;; Change this to a max heap structure? and/or sort the list by fitness
(defmethod get-fittest ((self mastermind-population)) 
	(setf best-individual-value -1) 
	(setf best-individual-return nil)
	(loop for individual in (individuals self) do 
		(if (and (fitness individual) (> (fitness individual) best-individual-value)) 
			(progn 
				(setf best-individual-return individual) 
				(setf best-individual-value (fitness individual)))))
	(cond ((not (null best-individual-return)) best-individual-return))
	nil)

;; Applies mutation to entire current population, might want to do this when crossing over instead.
(defmethod apply-mutations ((self mastermind-population) mutation-rate) 
	(loop for individual in (individuals self) do 
		(if (<= (random 1.0) mutation-rate) 
		(mutate-individual individual))))


(defmethod create-new-individual-using-crossover ((self mastermind-population) 
	individual-1 individual-2) 
	(if (<= (random 1.0) 0.5) 
		(2-point-crossover self individual-1 individual-2) 
		(1-point-crossover self individual-1 individual-2)))
	

;; returns a list of the color values for the child.
(defmethod 1-point-crossover ((self mastermind-population) individual-1 individual-2) 
	(let ((crossover-point (+ (random (- (length (color-values individual-1)) 1)) 1))) 
		(append 
			(subseq (color-values individual-1) 0 crossover-point) 
			(subseq (color-values individual-2) crossover-point (length (color-values individual-1))))))

(defmethod 2-point-crossover ((self mastermind-population) individual-1 individual-2) 
	(let ((crossover-point (list 
		(random (length (color-values individual-1)))
		(random (length (color-values individual-1))))))
		(setf crossover-point (sort crossover-point #'<))
		(append 
			(subseq (color-values individual-1) 0 (first crossover-point)) 
			(subseq (color-values individual-2) (first crossover-point) (second crossover-point)) 
			(subseq (color-values individual-1) (second crossover-point) (length (color-values individual-1))))))

;; (A B C D E F)
;; (4 1)

;; Percentage based vs. Tournament Selection? RIGHT NOW ITS JUST RANDOM!
(defmethod selection-algorithm ((self mastermind-population)) 
	(let 
		((random-selection (random (length (individuals self))))) 
		(nth random-selection (individuals self))))

(defmethod tournament-selection ((self mastermind-population) tournament-size) 
	(let ((tournament-set 
		(loop for index from 0 to tournament-size collect 
			(nth (random (length (individuals self))) (individuals self))))) 
		(find-best-fitness-in-tournament tournament-set)))

(defun find-best-fitness-in-tournament (tournament-set) 
	(let ((min-so-far 999) 
		 (best-individual nil))
		(loop for individual in tournament-set do 
			(if (< (fitness individual) min-so-far) 
				(progn 
					(setf min-so-far (fitness individual)) 
					(setf best-individual individual)))) 
	best-individual))

(defun compare-individuals-greater-fitness (individual-1 individual-2) 
	(if (<= (fitness individual-1) (fitness individual-2)) 
		individual-2 
		individual-1))

(defun compare-individuals-lesser-fitness (individual-1 individual-2) 
	(if (>= (fitness individual-1) (fitness individual-2)) 
		individual-2 
		individual-1))





