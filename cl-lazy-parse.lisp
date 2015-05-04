(in-package #:cl-lazy-parse)

;;;;;;;;;; Basic lazy computation stuff 
;;; (avoiding calling them delay/force, because I suspect they ultimately won't be thunks)
(defstruct paused fn)
(defmacro pause (&body body)
  `(make-paused :fn (lambda () ,@body)))
(defmethod resume ((p paused))
  (funcall (paused-fn p)))

;;;;;;;;;; Parsers
;;; A parser is a function that might return a result, a +fail+ or a paused state.
(defmethod run! ((r rapid) (parser function))
  (funcall parser r))
(defmethod run! ((r rapid) (str string))
  (funcall (apply #'and>> (map 'list #'char>> str)) r))
(defmethod run! ((r rapid) (chr character))
  (funcall (char>> chr) r))

(defparameter +fail+ (gensym "FAIL"))
(defun failed? (thing) (eq thing +fail+))

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

;;;;;;;;;; Basic transformation
(defun with (parser fn)
  (lambda (r)
    (labels ((next! () (run! r parser))
	     (cont (v)
	       (cond ((paused-p v)
		      (pause (cont (resume v))))
		     ((failed? v) +fail+)
		     (t (apply fn v)))))
      (cont (next!)))))

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

(defmacro then (parser (&rest args) &body body)
  (multiple-value-bind (final-args ignored)
      (loop for a in args
	 for s = (gensym "IGNORED")
	 if (eq a '_) 
	 collect s into res and collect s into vars
	 else collect a into res
	 finally (return (values res vars)))
    `(with ,parser
	   (lambda ,final-args
	     (declare (ignore ,@ignored))
	     ,@body))))

;;;;;;;;;; Basic parsers
(defmethod char>> ((pred function))
  (lambda (r)
    (labels ((cont (v)
	       (cond ((paused-p v) 
		      (pause (cont (resume v))))
		     ((funcall pred v)
		      v)
		     (t 
		      (_push-peeked! r v)
		      +fail+))))
      (cont (run! r #'char!)))))

(defmethod char>> ((pred character))
  (char>> (lambda (c) (eql c pred))))
(defmethod char>> ((pred string))
  (let ((lst (coerce pred 'list)))
    (char>> (lambda (c) (member c lst)))))

(defun space? (c) (eql c #\space))
(defun non-space? (c) (not (space? c)))
(defun floating? (c) 
  (let ((code (char-code c)))
    (or (= code 46) (>= 57 code 48))))

;;;; Example
(defparameter *example* "GET /index.html HTTP/1.1
Host: www.example.com
Content-Length: 38

")

(defparameter +crlf+ (coerce (list #\return #\linefeed) 'string))

(defparameter http-method>> 
  (or>> "GET" "DELETE" "POST" "PUT"))

(defun to-string (seq)
  (coerce seq 'string))

(defparameter request-line>>
  (with (and>> http-method>> " " (many>> (char>> #'non-space?)) " HTTP/1.1" +crlf+)
	(_fn (method _ uri _ _)
	  (cons (to-string method) (to-string uri)))))

(defun header-char? (c)
  (let ((code (char-code c)))
    (or (= code 45) (>= 122 code 65))))
(defun header-val-char? (c) 
  (> (char-code c) 13))

(defparameter header>>
  (with (and>> (many>> (char>> #'header-char?)) ": " (many>> (char>> #'header-val-char?)) +crlf+)
	(_fn (k _ v _)
	  (cons (intern (string-upcase (to-string k)) :keyword)
		(to-string v)))))

(defparameter request>>
  (with (and>> request-line>>
	       (many>> header>>))
	(lambda (req headers)
	  (format t "~a~%" req)
	  (format t "~{   ~a~%~}" headers))))

(with-input-from-string (s *example*)
  (let ((r (rapid s)))
    (run! r request>>)))
