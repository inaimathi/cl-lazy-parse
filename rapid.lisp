(in-package #:cl-lazy-parse)

;;;;;;;;;; Buffer structure
(defstruct buffer arr size (end-ix 0) (read-ix 0))
(defun buffer (&key (initial-size 512))
  (make-buffer :arr (make-string initial-size) :size initial-size))

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

;;;;;;;;;; Rapids are streams that don't block on peek! or char! operations
(defclass rapid ()
  ((stream-of :reader stream-of :initarg :stream-of)
   (cached :reader cached :initform (buffer) :initarg :cached)))

(defmethod rapid ((s stream) &key (buffer-size 256))
  (make-instance 'rapid :stream-of s :cached (buffer :initial-size buffer-size)))

(defmethod getc! ((r rapid))
  (let ((res (read-char-no-hang (stream-of r))))
    (if res
	(place! (cached r) res)
	(pause (getc! r)))))

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

;;; Sugar
(defmacro with-rapid ((var stream) &body body)
  `(let ((,var (rapid ,stream)))
     ,@body))

(defmacro with-rapid-string ((var string) &body body)
  (let ((s (gensym "S")))
    `(with-input-from-string (,s ,string)
       (let ((,var (rapid ,s)))
	 ,@body))))
