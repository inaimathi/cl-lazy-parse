(in-package #:cl-lazy-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Parsers
;;; A parser is a function that might return a result, a +fail+ or a paused state.
(defmethod run! ((r rapid) (n null)) nil)
(defmethod run! ((r rapid) (parser function))
  (funcall parser r))
(defmethod run! ((r rapid) (chr character))
  (funcall (char>> chr) r))

;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make parsers recursively reversible, so that you can define the run! string fn as
;;  (funcall (apply #'and>> (map 'list #'char>> str)) r)
(defmethod run! ((r rapid) (str string))
  (funcall (string->parser str) r))

(defun string->parser (str)
  (let ((lst (coerce str 'list)))
    (lambda (r)
      (labels ((cont (v ct)
		 (cond ((paused-p v) 
			(pause (cont (resume v) ct)))
		       ((equal lst v)
			(coerce (loop repeat (length str) collect (char! r)) 'string))
		       ((and (> (length str) ct) (alexandria:starts-with-subseq v lst))
			(cont (peek! r :count (+ ct 1)) (+ ct 1)))
		       (t +fail+))))
	(cont (peek! r) 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +fail+ (gensym "FAIL"))
(defun failed? (thing) (eq thing +fail+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Now then, basic composition
(defun and>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
If any of them fail, the entire expression fails."
  (lambda (r)
    (let ((res nil)
	  (rest parsers))
      (labels ((next! () (run! r (pop rest)))
	       (cont (v)
		 (cond ((paused-p v)
			(pause (cont (resume v))))
		       ((failed? v)
			+fail+)
		       (rest
			(push v res)
			(cont (next!)))
		       (t
			(reverse (cons v res))))))
	(cont (next!))))))

(defun or>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
Returns the first successful one.
If they all fail, the entire expression fails."
  (lambda (r)
    (let ((rest parsers))
      (labels ((next! () (run! r (pop rest)))
	       (cont (v)
		 (cond ((paused-p v)
			(pause (cont (resume v))))
		       ((and rest (failed? v))
			(cont (next!)))
		       ((failed? v)
			+fail+)
		       (t v))))
	(cont (next!))))))

(defun many>> (parser)
  "Takes a parser and runs it until it fails.
Returns the accumulated successes (the empty list, if there were none)."
  (lambda (r)
    (let ((res nil))
      (labels ((next! () (run! r parser))
	       (cont (v)
		 (cond ((paused-p v)
			(pause (cont (resume v))))
		       ((failed? v) (reverse res))
		       (t
			(push v res)
			(cont (next!))))))
	(cont (next!))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic transformation
(defun with (parser fn)
  (lambda (r)
    (labels ((cont (v)
	       (cond ((paused-p v)
		      (pause (cont (resume v))))
		     ((failed? v) +fail+)
		     (t (apply fn v)))))
      (cont (run! r parser)))))

(defun on (parser fn)
  (lambda (r)
    (labels ((next! () (run! r parser))
	     (cont (v)
	       (cond ((paused-p v)
		      (pause (cont (resume v))))
		     ((failed? v) +fail+)
		     (t (funcall fn v)))))
      (cont (next!)))))

(defmacro _fn ((&rest args) &body body)
  (multiple-value-bind (final-args ignored)
      (loop for a in args
	 for s = (gensym "IGNORED")
	 if (eq a '_) 
	 collect s into res and collect s into vars
	 else collect a into res
	 finally (return (values res vars)))
    `(lambda ,final-args
       (declare (ignore ,@ignored))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic parsers
(defmethod char>> ((pred function))
  (lambda (r)
    (labels ((cont (v)
	       (cond ((paused-p v) 
		      (pause (cont (resume v))))
		     ((funcall pred v)
		      v)
		     (t 
		      (unchar! r v)
		      +fail+))))
      (cont (char! r)))))

(defmethod char>> ((pred character))
  (char>> (lambda (c) (eql c pred))))
(defmethod char>> ((pred string))
  (let ((lst (coerce pred 'list)))
    (char>> (lambda (c) (member c lst)))))
