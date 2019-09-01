;;;****************************************************************************************
;;;THIS PROVIDES THE FOUNDATION FOR CLOS AI SEARCH
;;;to be used concurrently with a problem file
;;;****************************************************************************************

;;;****************************************************************************************
;;;Search STATISTICS
;;;****************************************************************************************
(defclass search-statistics ()
  ((nodes-generated :initarg :nodes-generated :initform 0 :accessor nodes-generated)
   (nodes-expanded :initarg :nodes-expanded :initform 0 :accessor nodes-expanded)
   (maximum-frontier :initarg :maximum-frontier :initform 0 :accessor maximum-frontier)
   (length-of-solution :initarg :length-of-solution :initform 0 :accessor length-of-solution)
   (maximum-depth :initarg :maximum-depth :initform 0 :accessor maximum-depth)))
  
;;;****************************************************************************************
;;;CLASS DEFINITIONS
;;;****************************************************************************************

(defclass problem ()
  ((name :initarg :name :initform nil :accessor name)
   (start-state :initarg :start-state :initform nil :accessor start-state)
   (goal-test :initarg :goal-test :initform nil :accessor goal-test)
   (operators :initarg :operators :initform nil :accessor operators)
   (statistics :initarg :statistics :initform (make-instance 'search-statistics) :accessor statistics)))

(defmethod reset-statistics ((self problem))
  (setf (statistics self) (make-instance 'search-statistics)))
  
(defparameter *trace-search* nil)

;the following  should be redefined for each problem
(defclass state ()
  ())

(defmethod equal-states ((self state) (other state))
  ())

(defmethod copy-state ((self state))
  ())

(defmethod estimated-distance-from-goal ((self state))
  ())

(defmethod printer ((self state))
  ())

(defclass node ()
  ((state :initarg :state :initform nil :accessor state)
   (problem :initarg :problem :initform nil :accessor problem)
   (path :initarg :path :initform nil :accessor path)
   (ancestors :initarg :ancestors :initform nil :accessor ancestors)))
  
(defmethod update-statistics ((self problem) expand frontier)
  (let ((stats (statistics self)))
    (when (> (length frontier) (maximum-frontier stats))
      (setf (maximum-frontier stats) (length frontier)))
    (when (> (length (path expand)) (maximum-depth stats))
      (setf (maximum-depth stats) (length (path expand))))))

;;;****************************************************************************************
;;;GENERAL FUNCTIONS 
;;;****************************************************************************************

;adds atom to the end of list
(defun add-to-end (atom list)
  (append list (list atom)))

;finds the successor of state resulting from application of operator
(defun successor-state (state operator)
  (funcall operator state))

;makes successor node from successor of state
(defmethod successor-node ((self node) operator)
  (let ((next (successor-state (state self) operator)))
    (when next
      (make-instance 'node :state next :path (add-to-end operator (path self)) :problem (problem self)))))

(defmethod reached-the-goal ((self node))
  (funcall (goal-test (problem self)) (state self)))

;;;****************************************************************************************
;;;BREADTH FIRST SEARCH DEFINITIONS
;;;****************************************************************************************

(defmethod finish-successful-search ((self problem) expand)
  (setf (length-of-solution (statistics self)) (length (path expand)))
  (describe (statistics self))
  (format t "%")
  (describe expand)
  expand)

;;generated is a list of states generated
;;expanded is a list of states expanded
;;frontier is a list of nodes to be expanded
(defmethod breadth-first-search ((self problem)) 
  (reset-statistics self)
  (format t "~%Performing breadth first search on problem ~a.~%" (name self))
  (let* ((initial-state (start-state self))
	 (frontier (list (make-instance 'node :state initial-state :path nil :problem self)))
	 (operators (operators self))
	 (generated (list initial-state))
	 solved expanded expand)
    (loop until (or solved (null frontier))
       do (setf expand (pop frontier)) 
	 (incf (nodes-expanded (statistics self)))
	 (push (state expand) expanded) 
	 (when *trace-search* (format t "~%~%Exploring ~a" (describe expand)))
	 (loop for operator in operators
	    for child = (successor-node expand operator) ;this is a node
	    for state = (when child (state child)) ;this is a state 
	    do (cond ((null child) nil)
		     ((reached-the-goal child)
		      (setf solved t)
		      (update-statistics self child frontier)
		      (finish-successful-search self child))
		     ((and (not (already-statep state expanded)) (not (already-nodep state frontier)))
		      (incf (nodes-generated (statistics self)))
		      (push state generated)
		      (setf frontier (add-to-end child frontier))
		      (update-statistics self child frontier))
		     (t nil)))
	 finally (when solved (return t)))))
  
(defparameter *trace-search* nil)

;;;****************************************************************************************
;;;DEPTH FIRST SEARCH DEFINITIONS
;;;****************************************************************************************

;;generated is a list of states generated
;;expanded is a list of states expanded
;;frontier is a list of nodes to be expanded
(defmethod depth-first-search ((self problem))
  (reset-statistics self)
  (format t "~%Performing depth first search on problem ~a.~%" (name self))
  (let* ((initial-state (start-state self))
	 (frontier (list (make-instance 'node :state initial-state :path nil :problem self)))
	 (operators (operators self))
	 (generated (list initial-state))
	 solved expanded expand) 
      (loop until (or solved (null frontier))
       do (setf expand (pop frontier)) ;(print (length (path expand))) (print (path expand)) uncomment to watch how deep you go
	 (incf (nodes-expanded (statistics self)))
	 (push (state expand) expanded)
	 (when *trace-search* (format t "~%~%Exploring ~a" (describe expand)))
	   (cond ((reached-the-goal expand)
		  (setf solved t) ;note that you don't stop if until you expand from a goal state
		  (finish-successful-search self expand))
	       (t (loop for operator in operators
		     for child = (successor-node expand operator)
		     when child ;whether or not it is a goal state
		     do (incf (nodes-generated (statistics self)))
		     and do (push (state child) generated) 
		     and do (setf frontier (push child frontier))
		     and do (update-statistics self child frontier))))
       finally (when solved (return t)))))

;state is equal to some state in list (there has to be an equal-states method defined)
(defun already-statep (state list)
  (member state list :test 'equal-states))

;state is equal to state of some node in list (there has to be an equal-states method defined)
(defun already-nodep (state list)
  (loop for node in list
       thereis (equal-states state (state node))))

;keep a list of states alredy visited
(defmethod depth-first-search-with-duplicate-node-detection ((self problem))
  (reset-statistics self)
  (format t "~%Performing depth first search with duplicate node detection on problem ~a.~%" (name self))
  (let* ((initial-state (start-state self))
         (frontier (list (make-instance 'node :state initial-state :path nil :problem self)))
         (operators (operators self))
         (generated (list initial-state))
         solved expanded expand)
    (loop until (or solved (null frontier))
       do (setf expand (pop frontier))
	 (incf (nodes-expanded (statistics self)))
	 (setf expanded (cons (state expand) expanded))
	 (when *trace-search* (format t "~%~%Exploring ") (describe expand))
	 (cond ((reached-the-goal expand)
		(setf solved t)
		(finish-successful-search self expand))
	       (t (loop for operator in operators
		     for child = (successor-node expand operator)
		     when (and child (not (already-statep (state child) expanded))) ;; BIG difference
		     do (incf (nodes-generated (statistics self)))
		     and do (push (state child) generated) 
		     and do (setf frontier (push child frontier))
		     and do (update-statistics self child frontier))))
       finally (when solved (return t)))))

;set a global depth limit
(defparameter *default-depth-limit* 50)

;don't explore if length is greater than limit
;includes duplicate detection
(defmethod depth-first-search-with-depth-limit ((self problem))
  (reset-statistics self)
  (format t "~%Performing depth first search with depth limit on problem ~a.~%" (name self))
  (let* ((initial-state (start-state self))
         (frontier (list (make-instance 'node :state initial-state :path nil :problem self)))
         (operators (operators self))
         (generated (list initial-state))
	 (depth-limit *default-depth-limit*)
	 solved expanded expand)
    (loop until (or solved (null frontier))
       do (setf expand (pop frontier))
	 (incf (nodes-expanded (statistics self)))
	 (setf expanded (cons (state expand) expanded))
	 (cond ((reached-the-goal expand)
		(setf solved t)
 		(finish-successful-search self expand))
               ((< (length (path expand)) depth-limit) ;; BIG difference
		(loop for operator in operators
		     for child = (successor-node expand operator)
		     when (and child (not (already-statep (state child) expanded))) ;; BIG difference
		     do (incf (nodes-generated (statistics self)))
		     and do (push (state child) generated) 
		     and do (setf frontier (push child frontier))
		     and do (update-statistics self child frontier)))
	       (t nil))
       finally (when solved (return t)))))

;;;****************************************************************************************
;;;ADDITIONAL BASIC DEFINITIONS FOR HEURISTIC SEARCH
;;;****************************************************************************************

(defclass heuristic-node (node)
  ((estimated-distance-from-goal :initarg :estimated-distance-from-goal
                                 :initform nil
                                 :accessor estimated-distance-from-goal)
   (cost-of-plan-so-far :initarg :cost-of-plan-so-far
                        :initform 0
                        :accessor cost-of-plan-so-far)))

(defun cost-of-applying-operator (state operator) 
  (declare (ignore state operator))
  1)

(defmethod successor-node ((self heuristic-node) operator)
  (let ((next (successor-state (state self) operator)))
    (when next
      (make-instance 'heuristic-node :state next :path (add-to-end operator (path self))
        :estimated-distance-from-goal (estimated-distance-from-goal next)
        :problem (problem self)
        :cost-of-plan-so-far (+ (cost-of-plan-so-far self) 
                                (cost-of-applying-operator (state self) operator))))))

;;;****************************************************************************************
;;;BEST FIRST SEARCH DEFINITIONS
;;;****************************************************************************************

(defun sort-by-estimated-distance (list)
  (sort list #'< :key 'estimated-distance-from-goal))

(defmethod best-first-search ((self problem))
    (reset-statistics self)
    (format t "~%Performing best first search on problem ~a.~%" (name self))
  (let* ((initial-state (start-state self))
         (frontier (list (make-instance 'heuristic-node  :state initial-state :path nil :problem self :cost-of-plan-so-far 0
					:estimated-distance-from-goal (estimated-distance-from-goal initial-state))))
         (operators (operators self))
         solved expanded expand)
    (loop until (or solved (null frontier))
          do (setf expand (pop frontier)) 
	 (incf (nodes-expanded (statistics self)))
	 (setf expanded (cons (state expand) expanded)) 
	 (when *trace-search* (format t "~%~%Exploring ~a" (describe expand)))
	 (cond ((reached-the-goal expand)
		(setf solved t)
		(finish-successful-search self expand))
	       (t (loop for operator in operators
		     for child = (successor-node expand operator)
		     when child
		     do (incf (nodes-generated (statistics self)))
		     when (and child (not (already-statep (state child) expanded)))
		     do (push (state child) expanded) 
		     and do (setf frontier (push child frontier))
		     and do (setf frontier (sort-by-estimated-distance frontier))
		     and do (update-statistics self child frontier))))
       finally (when solved (return t)))))
                        
;;;****************************************************************************************
;;;STEEPEST ASCENT HILL CLIMBING SEARCH DEFINITIONS
;;;****************************************************************************************

(defmethod steepest-ascent-hill-climbing ((self problem))
  (let* ((initial-state (start-state self))
         (operators (operators self))
	 (minimum-distance-to-goal 0)
	 current-node solved best-successor)
    (reset-statistics self)
    (format t "~%Performing steepest ascent hill climbing search on problem ~a.~%" (name self))
    (loop until (or (null current-node) solved)
       initially (setf current-node
		       (make-instance 'heuristic-node :state initial-state :path nil :problem self
				      :cost-of-plan-so-far 0 :estimated-distance-from-goal (estimated-distance-from-goal initial-state)))
       do (setf minimum-distance-to-goal (estimated-distance-from-goal current-node))
	 (cond ((reached-the-goal current-node)
		(setf solved t)
		(finish-successful-search self current-node))
	       (t (loop for operator in operators
		     for child = (successor-node current-node operator)
		     do (setf best-successor nil) 
		     when child 
		     do (incf (nodes-generated (statistics self)))
		     when (and child (< (estimated-distance-from-goal child) minimum-distance-to-goal))
		     do (setf best-successor child)
		     and do (setf minimum-distance-to-goal (estimated-distance-from-goal child))
		     and do (setf current-node best-successor)
		     finally (unless best-successor (setf current-node nil)))))
       finally (when solved (return t)))))

;;;****************************************************************************************
;;;OPTIMAL HEURISTIC SEARCH DEFINITIONS
;;;****************************************************************************************

(defun sort-by-cost-so-far (list)
  (sort list #'< :key 'cost-of-plan-so-far))

(defmethod optimal-heuristic-search ((self problem))
    (reset-statistics self)
    (format t "~%Performing optimal heuristic search on problem ~a.~%" (name self))
  (let* ((initial-state (start-state self))
         (frontier (list (make-instance 'heuristic-node :state initial-state :path nil :problem self :cost-of-plan-so-far 0
					:estimated-distance-from-goal (estimated-distance-from-goal initial-state))))
	 (operators (operators self))
	 solved expanded expand)
    (loop until (or solved (null frontier))
       do (setf expand (pop frontier))
	 (incf (nodes-expanded (statistics self)))
	 (setf expanded (cons (state expand) expanded))
	 (when *trace-search* (format t "~%~%Exploring ~a" (describe expand)))
	 (cond ((reached-the-goal expand)
		(setf solved t)
		(finish-successful-search self expand))
	       (t (loop for operator in operators
		     for child = (successor-node expand operator)
		     for state = (when child (state child)) ;this is a state 
		     when child
		     do (incf (nodes-generated (statistics self)))
		     when (and child (not (member state expanded :test 'equal-states)))
		     do (push state expanded)
		     and do (setf frontier (push child frontier))
		     and do (setf frontier (sort-by-cost-so-far frontier))
		     and do (update-statistics self child frontier))))
       finally (when solved (return t)))))
                                                

;;;****************************************************************************************
;;;A-STAR SEARCH DEFINITIONS
;;;****************************************************************************************


(defclass a-star-node (heuristic-node)
  ())

(defmethod successor-node ((self a-star-node) operator)
  (let ((next (successor-state (state self) operator)))
    (when next
      (make-instance 
        'a-star-node :state next :path (add-to-end operator (path self)) :problem (problem self)
        :ancestors (cons self (ancestors self))
        :estimated-distance-from-goal (estimated-distance-from-goal next)
        :cost-of-plan-so-far (+ (cost-of-plan-so-far self) 
                                (cost-of-applying-operator (state self) operator))))))

(defun a-star-value (node)
  (+ (estimated-distance-from-goal node) (cost-of-plan-so-far node)))

(defun sort-by-cost-plus-plan (list)
  (sort list #'< :key 'a-star-value))

(defun equal-states-in-nodes (node-1 node-2)
  (equal-states (state node-1) (state node-2)))

;returns t if some old node on list has a longer path to node
(defun shorter-pathp (node list)
  (loop for old in list
        thereis (and (equal-states-in-nodes node old)
                     (< (cost-of-plan-so-far node) (cost-of-plan-so-far old)))))

;returns 3 item list of 
;(nodes in list before one equivalent to node, equivalent node, list of nodes after)
(defun segment-path (node list)
  (let ((found nil)
        (equivalent nil)
        (answer nil))
    (setf answer
          (loop for old in list 
                when (and (not found) (equal-states (state node) (state old))
                          (< (cost-of-plan-so-far node) (cost-of-plan-so-far old)))
                do (setf found t)
                and do (setf equivalent old)
                else collect old into before
                when (and found (not (equal-states (state node) (state old))))
                collect old into after
                finally (return (list before equivalent after))))
    (if found answer nil)))

;corrects path and path length of any old node in list that has an ancestor equivalent to node 
(defun correct-paths (node list)
  (loop for old in list
        for segments = (segment-path node (ancestors old))
        for equivalent = (second segments)
        for after = (third segments)
        unless (null equivalent)
        do (setf (ancestors old) (append (ancestors node) (list node) after))
        and do (setf (cost-of-plan-so-far old) 
                     (+ (cost-of-plan-so-far old) 
                        (cost-of-plan-so-far node) 
                        (- (cost-of-plan-so-far equivalent))))))

(defun already-visited-heuristicp (node visited)
  (member node visited :test 'equal-states-in-nodes))

;;expanded is nodes
;;generated is nodes
;;frontier is nodes
(defmethod a-star-search ((self problem))
    (reset-statistics self)
    (format t "~%Performing a* search on problem ~a.~%" (name self))
  (let* ((initial-state (start-state self))
         (frontier (list (make-instance 'a-star-node :problem self :state initial-state :path nil :cost-of-plan-so-far 0
					      :estimated-distance-from-goal (estimated-distance-from-goal initial-state))))
	 (operators (operators self))
	 solved expanded expand)
    (loop until (or solved (null frontier))
       do (setf expand (pop frontier)) 
	 (incf (nodes-expanded (statistics self)))
	 (setf expanded (cons expand expanded))
	 (when *trace-search* (format t "~%~%Exploring ~a" (describe expand)))
	 (cond ((reached-the-goal expand)
		(setf solved t)
		(finish-successful-search self expand))
	       (t (loop for operator in operators
		     for child = (successor-node expand operator) ;this is a node
		     when child 
		     do (incf (nodes-generated (statistics self))) ;and do (describe (state child))
		     when (and child (not (already-nodep (state child) expanded)))
		     do (setf frontier (push child frontier))
		     and do (update-statistics self child frontier)
		     else when (and child (shorter-pathp child expanded))
		     do (correct-paths child (append expanded frontier))
		     and do (setf frontier (sort-by-cost-plus-plan frontier)))))
       finally (when solved (return t)))))
 
