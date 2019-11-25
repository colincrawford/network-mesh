(defpackage :message-mesh
  (:use :common-lisp))

(in-package :message-mesh)

;; Simple FIFO Queue implementation
(defclass queue ()
  ((start
     :initform 0
     :initarg :start
     :accessor start)
   (items
    :initform (make-array 10 :adjustable t :fill-pointer 0)
    :accessor items)))

(defun make-queue (&optional (items '()))
  (let ((queue (make-instance 'queue)))
    (loop for item in items
       do (enqueue queue item))
    queue))

(defmethod enqueue ((queue queue) item)
  (with-slots (items)
      queue
    (vector-push item items)))

(defmethod dequeue ((queue queue))
  (with-slots (start items)
      queue
    (if (<= (length items) start)
	(error "Dequeue called on an empty queue")
	(let ((item (aref items start)))
	  (incf start)
	  item))))

(defmethod empty ((queue queue))
  (with-slots (start items)
      queue
    (> (+ 1 start) (length items))))

;; - Algorithm -
;; iterate breadth first from both directions
;; (keeping track of the name and route to get there as we go)
;; -----------------
;; from the starting node forward
;; from the ending node backwards
;; -----------------
;; we'll alternate the direction at each round
;;
;; Termination conditions
;; 1. The next node (name) has already been seen from the other direction
;;    - We've finished the route and can combine the two routes to get the full route
;; 2. We cant progress (from either direction) to an unseen name
;;    - (unseen from that same direction)
(defun route (from to network)
  (let* ((seen-left (make-hash-table :test #'equal))
	 (seen-right (make-hash-table :test #'equal))
	 (next-left (make-queue `((,from ()))))
	 (next-right (make-queue `((,to ()))))
	 ;; we'll update this every round
	 (direction :left)
	 (solution-path))
    (loop while (and (not (and (empty next-left) (empty next-right)))
		     (not solution-path))
       do (let*  ((name-queue (if (equal direction :right) next-right next-left))
		  (next (dequeue name-queue))
		  (next-name (car next))
		  (next-path (second next))
		  (seen (if (eq direction :right) seen-right seen-left))
		  (seen-other-side (if (eq direction :right) seen-left seen-right))
		  (edges (gethash next-name network))
		  (path-from-other-side (gethash next-name seen-other-side))
		  (path (if (eq direction :right)
			    (cons next-name next-path)
			    (append next-path `(,next-name)))))
	    (setf (gethash next-name seen) path)
	    (if (nth-value 1 (gethash next-name seen-other-side))
		(setf solution-path (if (equal direction :right)
					(append path-from-other-side (cdr path))
					(append path (cdr path-from-other-side))))
		(loop for edge in edges
		   when (not (nth-value 1 (gethash edge seen)))
		   do (enqueue name-queue `(,edge ,path))))
	    (setf direction (if (eq direction :right) :left :right))))
    solution-path))


;; Tests
(defun test (test-name result expected-result)
  (format t "~A~%" test-name)
  (if (equal result expected-result)
      (format t "Passed!~%")
      (format t "Failed! ~%Got: ~A~%Expected: ~A~%" result expected-result)))

(defun test-network ()
  (let ((input (make-hash-table :test #'equal))
	(result))
    (setf (gethash "Min" input) '("William" "Jayden" "Omar"))
    (setf (gethash "William" input) '("Min" "Noam"))
    (setf (gethash "Jayden" input) '("Min" "Amelia" "Ren" "Noam"))
    (setf (gethash "Ren" input) '("Jayden" "Omar"))
    (setf (gethash "Amelia" input) '("Jayden" "Adam" "Miguel"))
    (setf (gethash "Adam" input) '("Amelia" "Miguel" "Sofia" "Lucas"))
    (setf (gethash "Miguel" input) '("Amelia" "Adam" "Liam" "Nathan"))
    (setf (gethash "Noam" input) '("Nathan" "Jayden" "William"))
    (setf (gethash "Omar" input) '("Ren" "Min" "Scott"))

    (setf result (route "Jayden" "Adam" input))
    
    (test "Normal Network" result '("Jayden" "Amelia" "Adam"))))

(defun test-unconnected-names ()
  (let ((input (make-hash-table :test #'equal))
	(result))
    (setf (gethash "William" input) '("Min" "Noam"))
    (setf (gethash "Josh" input) '("John"))
    (setf result (route "William" "Josh" input))
    
    (test "Unconnected Names" result nil)))

(defun test-two-connected-names ()
  (let ((input (make-hash-table :test #'equal))
	(result))
    (setf (gethash "Min" input) '("John"))
    (setf (gethash "John" input) '("Min"))

    (setf result (route "Min" "John" input))
    
    (test "Two Connected Names" result '("Min" "John"))))

(defun run-tests ()
  (let ((tests '(test-network
		 test-unconnected-names
		 test-two-connected-names)))
    (loop for test in tests do (funcall test))))
