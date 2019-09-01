;; use this with setf! 
(defun create-new-individual (num-pegs num-colors) 
	(let ((new-individual (make-instance 'mastermind-individual :num-pegs num-pegs 
		:colors (firstn num-colors '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z))))) 
	(setf (color-values new-individual) (generate-random-individual new-individual))
	new-individual))

(defclass mastermind-individual ()
    ((num-pegs :initarg :num-pegs :initform nil :accessor num-pegs :documentation "The number of pegs in the problem")
    (colors :initarg :colors :initform nil :accessor colors :documentation "The list of colors in problem")
    (color-values :initarg :color-values :initform nil :accessor color-values :documentation "The genes of the individual!")
    (fitness :initarg :fitness :initform nil :accessor fitness :documentation "The fitness of the individual")))

;; (defun insert-colors (length colors)
;;   (loop for i from 1 to length
;;      collect (random-chooser colors)))

;; creates a random individual. 
(defmethod generate-random-individual ((self mastermind-individual))
	(setf color-values (loop for i from 1 to (num-pegs self) 
		collect (random-chooser-ind (colors self)))))

(defmethod get-color-at-index ((self mastermind-individual) index) 
	(nth (color-values self) index))

(defmethod set-color-at-index ((self mastermind-individual) index color) 
	(setf (nth (color-values self) index) color))

;; If fitness is nil, then calculate if not, return.
(defmethod get-fitness ((self mastermind-individual)) 
	(if (eql (fitness self) nil) 
		(setf (fitness self) (calculate-fitness (color-values self)))
		(fitness self)))

;; Mutates one random index with a random color. 
;; WIP , add permutation and inversion? with probabilities x
(defmethod mutate-individual-or-not ((self mastermind-individual) chance-to-mutate) 
	(if (<= (random 1.0) chance-to-mutate) 
		(progn 
		(mutate-individual self) 
		;(print "Mutation!") 
		self)  
	self))

(defmethod to-permutate-or-not-to-permutate ((self mastermind-individual) permutation-rate) 
	(if (<= (random 1.0) permutation-rate) 
		(progn 
			(permute-individual self) 
			self) 
		self))

(defmethod permute-individual ((self mastermind-individual)) 
	(let ((two-points (list 
		(random (length (color-values self)))
		(random (length (color-values self)))))) 
		(rotatef (nth (first two-points) (color-values self)) (nth (second two-points) (color-values self))))
	(color-values self))

(defmethod to-invert-or-not-to-invert ((self mastermind-individual) inversion-rate) 
	(if (<= (random 1.0) inversion-rate) 
		(progn 
			(invert-individual self) 
			self) 
		self))

(defmethod invert-individual ((self mastermind-individual)) 
	(let ((two-points (list 
		(random (length (color-values self)))
		(random (length (color-values self)))))) 
		(setf two-points (sort two-points #'<))
		(append 
			(subseq (color-values self) 0 (first two-points)) 
			(reverse (subseq (color-values self) (first two-points) (second two-points)))
			(subseq (color-values self) (second two-points) (length (color-values self))))))

(defmethod mutate-individual ((self mastermind-individual)) 
	(setf (nth (random (length (color-values self))) (color-values self)) (random-chooser-ind (colors self))))

(defun random-chooser-ind (list)
  (nth (random (length list)) list))

;; ; get first n in list
;; (defun firstn (number list)
;;   (loop for i from 1 to number
;;      for item in list
;;      collect item))

