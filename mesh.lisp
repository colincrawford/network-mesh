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
	 (next-left (make-instance 'queue))
	 (next-right (make-instance 'queue))
	 ;; we'll update this every round
	 (direction :left)
	 (add-seen (lambda (direction name path-to)
		     (if (eq direction :right)
			 (setf (gethash name seen-right) (cons name path-to))
			 (setf (gethash name seen-left) (append path-to `(,name))))))
	 (add-next (lambda (direction name path-to)
		     (if (eq direction :right)
			 (enqueue next-right `(,name ,path-to))
			 (enqueue next-left `(,name ,path-to)))))
	 (combine-paths (lambda (direction path other-path)
			  (if (equal direction :right)
			      (append other-path (cdr path))
			      (append path (cdr other-path)))))
	 (path nil))
    (enqueue next-left `(,from (,from)))
    (enqueue next-right `(,to (,to)))
    ;; start with the beginning and ending nodes
    (funcall add-seen :left from '())
    (funcall add-seen :right to '())
    (loop while (and (not (and (empty next-left) (empty next-right)))
		     (not path))
       do (let*  ((next (if (equal direction :right)
			    (dequeue next-right)
			    (dequeue next-left)))
		  (next-name (car next))
		  (next-path (second next))
		  (seen (if (eq direction :right) seen-right seen-left))
		  (seen-other-side (if (eq direction :right) seen-left seen-right))
		  (edges (gethash next-name network))
		  (path-from-other-side (gethash next-name seen-other-side)))
	    (if (nth-value 1 (gethash next-name seen-other-side))
	      (let ((combined-path (funcall combine-paths direction next-path path-from-other-side)))
		(setf path combined-path))
	      (dolist (edge (remove-if (lambda (e) (gethash e seen)) edges))
		(let ((updated-path (if (equal direction :right)
					(cons edge next-path)
					(append next-path `(,edge)))))
		  (funcall add-next direction edge updated-path))))
	    (setf direction (if (eq direction :right) :left :right))))
    path))


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
