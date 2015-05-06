(in-package #:cl-lazy-parse)

;;;;;;;;;; Rapids are streams that don't block
;; TODO: Make peeking efficient.
(defclass rapid ()
  ((stream-of :reader stream-of :initarg :stream-of)
   (peeked :accessor peeked :initform (queue))))

(defmethod peeked-ct ((r rapid)) (len (peeked r)))

(defmethod push-peeked! ((r rapid) (c character))
  (push! c (peeked r)))

(defmethod pop-peeked! ((r rapid))
  (pop! (peeked r)))

(defmethod rapid ((s stream))
  (make-instance 'rapid :stream-of s))

(defmethod getc! ((s stream))
  (let ((res (read-char-no-hang s)))
    (or res (pause (getc! s)))))

(defmethod getc! ((r rapid))
  (getc! (stream-of r)))

(defmethod take ((n integer) (lst list))
  (loop repeat n for elem in lst collect elem))
(defmethod take ((n integer) (q queue))
  (take n (messages q)))

;;;;;;;;;; External interface
;;; Basic calls
(defmethod peek! ((r rapid) &key (count 1))
  (assert (> count 0))
  (if (>= (peeked-ct r) count)
      (take count (peeked r))
      (let ((s (stream-of r)))
	(labels ((cont (v ct)
		   (cond ((paused-p v)
			  (pause (cont (resume v) ct)))
			 ((= ct 0)
			  (push-peeked! r v)
			  (take count (peeked r)))
			 (t
			  (push-peeked! r v)
			  (cont (getc! s) (- ct 1))))))
	  (cont (getc! s) (- count 1 (peeked-ct r)))))))

(defmethod char! ((r rapid))
  (cond ((> (peeked-ct r) 0)
	 (pop-peeked! r))
	(t (getc! (stream-of r)))))

(defmethod unchar! ((r rapid) (c character))
  (push-peeked! r c))

;;; Sugar
(defmacro with-rapid ((var stream) &body body)
  `(let ((,var (rapid ,stream)))
     ,@body))

(defmacro with-rapid-string ((var string) &body body)
  (let ((s (gensym "S")))
    `(with-input-from-string (,s ,string)
       (let ((,var (rapid ,s)))
	 ,@body))))
