;;;; package.lisp

(defpackage #:cl-lazy-parse
  (:use #:cl #:usocket)
  (:shadow #:get)
  (:export #:and>> #:or>> #:many>> #:char>> #:not-char>>
	   #:with #:_fn #:failed?))
