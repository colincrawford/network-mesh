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

;; Route:
;; find the shortest route from :from to :to given a
;; network. The network is a mapping from nodes to their
;; edges.
;; ex: John - Kyle, Clay
;;     Tyler - John, Matt
;;     Kyle - John
;;     Clay - Matt
;;     Matt - Kyle, Tyler
;;
;; Algorithm:
;; Treat the network as a graph and do breadth first search.
;; To speed up the completion of the correct path, do the search
;; from the start and end at the same time.
;;
;; Search Termination Scenarios:
;; 1. We find a name we've seen from the other side of the
;;    search. We can connect the two paths for our solution.
;; 2. We can't progress to an unseen node (unseen on the same
;;    side of the search) on either search. We've hit the end
;;    of the search from one side and there isn't an answer.
(defun route (from to network)
  (let* ((seen-left (make-hash-table :test #'equal))
	 (seen-right (make-hash-table :test #'equal))
	 (next-left (make-queue `((,from ()))))
	 (next-right (make-queue `((,to ()))))
	 (direction :left)
	 (solution-path))
    (loop until (or (empty next-left) (empty next-right) solution-path)
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
