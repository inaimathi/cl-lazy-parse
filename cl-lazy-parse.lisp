(in-package #:cl-lazy-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Parsers
;;; A parser is a function that might return a result, a +fail+ or a paused state.
(defmethod run! ((r rapid) (n null)) nil)
(defmethod run! ((r rapid) (parser function))
  (funcall parser r))
(defmethod run! ((r rapid) (chr character))
  (funcall (char>> chr) r))
(defmethod run! ((r rapid) (str string))
  (funcall (apply #'and>> (map 'list #'char>> str)) r))

(defmethod run! ((s stream) parser)
  (run! (rapid s) parser))
(defmethod run! ((path pathname) parser)
  (with-open-file (s path)
    (run! s parser)))
(defmethod run! ((str string) parser)
  (with-input-from-string (s str)
    (run! s parser)))

(defparameter +fail+ (gensym "FAIL"))
(defun failed? (thing) (eq thing +fail+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Now then, basic composition
(defun and>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
If any of them fail, the entire expression fails."
  (lambda (r)
    (let ((acc nil)
	  (total 0)
	  (rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
	       (cont (v &optional (ct 0))
		 (cond ((paused-p v)
			(pause (multiple-value-call #'cont (resume v))))
		       ((failed? v)
			(unread! r :count total)
			+fail+)
		       (rest
			(push v acc)
			(incf total ct)
			(next!))
		       (t
			(values (reverse (cons v acc)) (+ total ct))))))
	(next!)))))

(defun or>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
Returns the first successful one.
If they all fail, the entire expression fails."
  (lambda (r)
    (let ((rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
	       (cont (v &optional (ct 0))
		 (cond ((paused-p v)
			(pause (multiple-value-call #'cont (resume v))))
		       ((and rest (failed? v))
			(next!))
		       ((failed? v)
			+fail+)
		       (t (values v ct)))))
	(next!)))))

(defun many>> (parser)
  "Takes a parser and runs it until it fails.
Returns the accumulated successes (the empty list, if there were none)."
  (lambda (r)
    (let ((acc nil)
	  (total 0))
      (labels ((next! () (multiple-value-call #'cont (run! r parser)))
	       (cont (v &optional (ct 0))
		 (cond ((paused-p v)
			(pause (multiple-value-call #'cont (resume v))))
		       ((failed? v)
			(values (reverse acc) total))
		       (t
			(push v acc)
			(incf total ct)
			(next!)))))
	(next!)))))

(defun optionally>> (parser)
  "Takes a parser and runs it once. If it fails, returns NIL, otherwise, returns the given value."
  (lambda (r)
    (labels ((next! () (multiple-value-call #'cont (run! r parser)))
	     (cont (v &optional (ct 0))
	       (cond ((paused-p v)
		      (pause (multiple-value-call #'cont (resume v))))
		     ((failed? v)
		      (values nil 0))
		     (t
		      (values v ct)))))
      (next!))))

(defun if-internal>> (thunk a b)
  (lambda (r) (run! r (if (funcall thunk) a b))))

(defmacro if>> (condition a &optional b)
  `(if-internal>> (lambda () ,condition) ,a ,b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic transformation
(defun with (parser fn)
  (lambda (r)
    (labels ((cont (v &optional (ct 0))
	       (cond ((paused-p v)
		      (pause (multiple-value-call #'cont (resume v))))
		     ((failed? v) +fail+)
		     ((listp v)
		      (values (apply fn v) ct))
		     (t (values (funcall fn v) ct)))))
      (multiple-value-call #'cont (run! r parser)))))

(defun with-log (parser message)
  (with parser 
	(lambda (&rest ln)
	  (format t "~a ~s~%" message ln))))

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
		      (values v 1))
		     (t 
		      (unchar! r v)
		      +fail+))))
      (cont (char! r)))))

(defmethod char>> ((pred character))
  (char>> (lambda (c) (eql c pred))))
(defmethod char>> ((pred string))
  (let ((lst (coerce pred 'list)))
    (char>> (lambda (c) (member c lst)))))

(defun none-of>> (str)
  (char>> (lambda (c) (not (find c str)))))

(defun not-char>> (char)
  (char>> (lambda (c) (not (eql c char)))))
