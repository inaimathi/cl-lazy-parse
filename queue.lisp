(in-package #:cl-lazy-parse)

;;;;;;;;;; A Queue. Because, yeah, we kinda need it.
;;; Ripped bleeding from cl-wolf. Maybe I should just write a queue package and be done with it?
(defclass queue ()
  ((messages :accessor messages :initform nil)
   (last-cons :accessor last-cons :initform nil)
   (len :accessor len :initform 0)))

(defmethod push! (msg (q queue))
  (let ((m (list msg)))
    (if (empty? q)
	(setf (messages q) m
	      (last-cons q) (messages q))
	(setf (cdr (last-cons q)) m
	      (last-cons q) m))
    (incf (len q))
    nil))

(defmethod pop! ((q queue))
  (if (empty? q)
      (setf (last-cons q) nil)
      (progn (decf (len q))
	     (pop (messages q)))))

(defmethod empty? ((q queue))
  (null (messages q)))

(defun queue () (make-instance 'queue))
