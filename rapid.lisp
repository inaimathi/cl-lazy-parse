(in-package #:cl-lazy-parse)

;;;;;;;;;; Rapids are streams that don't block
;; TODO: Make peeking efficient.
(defclass rapid ()
  ((stream-of :reader stream-of :initarg :stream-of)
   (peeked :accessor peeked :initform (queue))))

(defmethod peeked-ct ((r rapid)) (len (peeked r)))

(defmethod _push-peeked! ((r rapid) (c character))
  (push! c (peeked r)))

(defmethod _pop-peeked! ((r rapid))
  (pop! (peeked r)))

(defmethod rapid ((s stream))
  (make-instance 'rapid :stream-of s))

(defmethod _getc! ((s stream))
  (let ((res (read-char-no-hang s)))
    (or res (pause (_getc! s)))))

(defmethod _getc! ((r rapid))
  (_getc! (stream-of r)))

(defmethod take ((n integer) (lst list))
  (loop repeat n for elem in lst collect elem))
(defmethod take ((n integer) (q queue))
  (take n (messages q)))

(defmethod peek! ((r rapid) &key (count 1))
  (assert (> count 0))
  (if (>= (peeked-ct r) count)
      (take count (peeked r))
      (let ((s (stream-of r)))
	(labels ((cont (v ct)
		   (cond ((paused-p v)
			  (pause (cont (resume v) ct)))
			 ((= ct 0)
			  (_push-peeked! r v)
			  (take count (peeked r)))
			 (t
			  (_push-peeked! r v)
			  (let ((res (read-char-no-hang s)))
			    (if res
				(cont res (- ct 1))
				(cont (pause (_getc! s)) ct)))))))
	  (cont (_getc! s) (- count 1 (peeked-ct r)))))))

(defmethod char! ((r rapid))
  (cond ((> (peeked-ct r) 0)
	 (_pop-peeked! r))
	(t (_getc! (stream-of r)))))
