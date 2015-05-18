(in-package #:cl-lazy-parse)

;;;;;;;;;; Buffer structure
(defstruct buffer arr (end-ix 0) (read-ix 0))
(defun buffer (&key (initial-size 512))
  (make-buffer :arr (make-string initial-size)))

(defmethod more? ((b buffer)) 
  (and (> (buffer-end-ix b) (buffer-read-ix b)) (> (buffer-end-ix b) 0)))

(defmethod place! ((b buffer) (c character))
  (let ((arr (buffer-arr b)))
    (when (= (buffer-end-ix b) (length arr))
      (setf (buffer-arr b) 
	    (concatenate 'string arr (make-string (length arr))))))
  (setf (aref (buffer-arr b) (buffer-end-ix b)) c)
  (incf (buffer-end-ix b))
  c)

(defmethod read! ((b buffer))
  (when (more? b)
    (let ((c (aref (buffer-arr b) (buffer-read-ix b))))
      (incf (buffer-read-ix b))
      c)))

(defmethod unread! ((b buffer) &key (count 1))
  (setf (buffer-read-ix b)
	(max 0 (- (buffer-read-ix b) count))))

(defmethod clear! ((b buffer))
  (setf (buffer-read-ix b) 0
	(buffer-end-ix b) 0))

;;;;;;;;;; Rapids are streams that don't block or char! operations, and might pause them instead
(defclass rapid ()
  ((stream-of :reader stream-of :initarg :stream-of)
   (cached :reader cached :initform (buffer) :initarg :cached)

   (max-age :reader max-age :initarg :max-age)
   (created :reader created :initform (get-universal-time))
   (max-pauses :reader max-pauses :initarg :max-pauses)
   (pauses :accessor pauses :initform 0)
   (allowance :accessor allowance :initarg :allowance)))

(defmethod rapid ((s stream) &key (buffer-size 256) (max-age 1000) (max-size 10000) (max-pauses 50))
  (make-instance 
   'rapid 
   :stream-of s :cached (buffer :initial-size buffer-size)
   :max-age max-age :allowance max-size :max-pauses max-pauses))

(define-condition rapid-error (error) ())
(define-condition rapid-too-long (rapid-error) ())
(define-condition rapid-too-slow (rapid-error) ())
(define-condition rapid-too-old (rapid-error) ())

(defmethod getc! ((r rapid))
  (cond 
    ((>= 0 (allowance r)) (error 'rapid-too-long))
    ((> (pauses r) (max-pauses r)) (error 'rapid-too-slow))
    ((> (- (get-universal-time) (created r)) (max-age r)) (error 'rapid-too-old)))
  (let ((res (read-char-no-hang (stream-of r))))
    (cond (res
	   (decf (allowance r))
	   (place! (cached r) res))
	  (t
	   (incf (pauses r))
	   (pause (getc! r))))))

;;;;;;;;;; External interface
;;; Basic calls
(defmethod char! ((r rapid))
  (unless (more? (cached r))
    (let ((res (getc! r)))
      (when (paused-p res)
	(pause (char! r)))))
  (read! (cached r)))

(defmethod unchar! ((r rapid) (c character))
  (declare (ignore c))
  (unread! (cached r))
  nil)

(defmethod unread! ((r rapid) &key (count 1))
  (unread! (cached r) :count count))
