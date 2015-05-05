(in-package :cl-lazy-parse)

;;;;;;;;;; Basic lazy computation stuff 
;;; (avoiding calling them delay/force, because I suspect they ultimately won't be thunks)
(defstruct paused fn)
(defmacro pause (&body body)
  `(make-paused :fn (lambda () ,@body)))
(defmethod resume ((p paused))
  (funcall (paused-fn p)))
