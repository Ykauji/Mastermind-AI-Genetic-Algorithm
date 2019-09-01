;; Original Fitness calculation from Lotte Berghman, Dries Goossens and Roel Leus.

(defclass mastermind-fitness-calculator ()
	;; unnecessary now!
    ((solution :initarg :solution :initform '(BLACK BLACK BLACK BLACK) :accessor solution 
    	:documentation "The solution of the GA")
    (previous-guesses :initarg :previous-guesses :initform nil :accessor previous-guesses 
    	:documentation "Previous guesses") 
    (weight-for-blacks :initarg :weight-for-blacks :initform 1 :accessor weight-for-blacks 
    	:documentation "The weight for the fitness calculation for black pins") 
    (weight-for-whites :initarg :weight-for-whites :initform 2 :accessor weight-for-whites 
    	:documentation "The weight for the fitness calculation for white pins") 
    (num-positions :initarg :num-positions :initform 4 :accessor num-positions
    	:documentation "The number of positions") 
    (num-colors :initarg :num-colors :initform 6 :accessor num-colors 
    	:documentation "The number of colors")))

(defvar *Mastermind-Fitness* (make-instance 'mastermind-fitness-calculator))

;; calculate the fitness of the individual
(defmethod calculate-fitness ((self mastermind-fitness-calculator) individual) 
	(if (not (previous-guesses self)) (return-from calculate-fitness nil))
	;(if (not (is-eligible-individual self individual)) (return-from calculate-fitness nil))
	(setf sum-blacks 0)
    (setf sum-whites 0)
    ;(setf sum-last-thingy (* (* weight-for-whites (num-positions self)) (- (third (second guess)) 1))) 
	(loop for guess in (previous-guesses self) do 
		(if (second guess) 
			(progn

				(setf sum-blacks (+ sum-blacks (fitness-formula-at-i-black self individual guess))) 
				(setf sum-whites (+ sum-whites (fitness-formula-at-i-white self individual guess))) 
				)))
	;(print (+ sum-blacks sum-whites))
	(+ sum-blacks sum-whites))

;; Second type of heuristic


;; C B A D
;; Previous guess = (A B C D) = (2,2,1)
;; F(A B D C) = (2,2) if ABCD was the secret code.
;;= weight-for-blacks * |(2-2)| + (|(2-2)| + weight-for-whites * num-positions(i-1)) = 0
;; F(C B A D) = (1,3) 
;; = 1 * |1-2| + |3-2| = 2	

; Fitness stuff. 
(defmethod fitness-formula-at-i-black ((self mastermind-fitness-calculator) individual guess) 
	(abs (- (first (process-guess-fitness self individual (first guess))) (first (second guess)))))

(defmethod fitness-formula-at-i-white ((self mastermind-fitness-calculator) individual guess) 
	(abs (- (second (process-guess-fitness self individual (first guess))) 
		(second (second guess)))))

;; Checks if the individual is a eligible pick!
(defmethod is-eligible-individual ((self mastermind-fitness-calculator) individual) 
	(loop for previous-guess in (previous-guesses self) do 
		(cond ((not (second previous-guess)) ())
			((not (equal (process-guess-fitness self individual (first previous-guess)) (list (first (second previous-guess)) (second (second previous-guess))))) 
			(progn 
				;(print (second previous-guess)) 
				;(print (process-guess-fitness self individual (first previous-guess)))
				(return-from is-eligible-individual nil)))
		(t ()))) 
	t)

;; Answer is A B C D
;; Guesses 
;; (C B A A) = (1 2)
;; (A A A A) = (1,0) 

;; Current sequence: (B D A C) <- make this secret code
;; (C B A A) = (1 2) 
;; (A A A A) = (1 0)

;; Current sequence: (C C B C) 
;; (A A A A) = (0 0)

(defmethod process-guess-fitness ((self mastermind-fitness-calculator) answer guess)
  (loop with answer = answer
     with guess-color-count = (color-counter-fitness self guess)
     with true-color-count = (color-counter-fitness self answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- (num-colors self))
					    for guessed = (aref true-color-count i)
					    for true = (aref guess-color-count i)
					    when (<= true guessed)
					    sum true
					    else sum guessed)))))

(defmethod scsa-scoring((self mastermind-fitness-calculator) list)
	; given an SCSA and a guess, return a heuristic value for the guess, higher values indicate the guess follows the SCSA
	(cond ((equal (SCSA *Mastermind*) "insert-colors") ()) 
		((equal (SCSA *Mastermind*) "two-color") ())
		((equal (SCSA *Mastermind*) "ab-color") ())
		((equal (SCSA *Mastermind*) "two-color-alternating") ())
		((equal (SCSA *Mastermind*) "only-once") ())
		((equal (SCSA *Mastermind*) "usually-fewer") ())
		((equal (SCSA *Mastermind*) "prefer-fewer") ())
		(T ()) ; is mystery SCSA
	))
; fitness function which discounts guesses with more than 2 colors
(defmethod two-color-fitness((self mastermind-fitness-calculator) list)
	(let ((colors-used NIL))
		(loop for color in list
			do (if (not (member color colors-used))
				(push color colors-used)))
	; look up if else syntax again
	(if (equal (length colors-used) 2) (return-from two-color-fitness -1)  ; reward length of 2,
		(return-from two-color-fitness (abs (- 2 (length colors-used))))))) ; punish distance from 2

(defmethod color-counter-fitness ((self mastermind-fitness-calculator) list)
  (loop with tally = (make-array (num-colors self) :initial-element 0)
     for peg in list
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))
;; Loop over all guesses, 

;; Fitness function :

;; f(C,i) = a(Σ from 1 to i, a(|X'q(c) - Xq|) + Σ from 1 to i | Y'q(c) - Yq | + bP(i - 1)
;; or Σ from 1 to i, -|X'q(c) - Xq| - |Y'q(c) - Yq | 
;; This makes the heuristic range from -x to 0. 0 being the highest value, aka eligible value!

;; C = individual 
;; i = current guess #
;; X'q = #Black pins correct
;; Y'q = #White pins correct 
;; a = weight of black pins
;; b = weight of white pins

;; General idea is that it iterates over all past guesses to compare the #of pins correct to find the 
;; new fitness. It's relative to it's past guesses! 
;; TO check for eligibility, kind Mr./Mrs. Berghman gives us a test on page 6 of the paper.
;; Also we may also use the rule based approach we designed earlier.