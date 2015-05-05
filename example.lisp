(in-package :cl-lazy-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Example
(defparameter *example* "GET /index.html HTTP/1.1
Host: www.example.com
Content-Length: 38

")

(defparameter *example2* "POST /index.html HTTP/1.1
Host: www.example.com
Content-Length: 38

")

(defun to-string (seq)
  (coerce seq 'string))

(defparameter crlf>>
  (to-string (list #\return #\linefeed)))

(defparameter http-method>> 
  (or>> "GET" "DELETE" "POST" "PUT"))

(defun space? (c) (eql c #\space))
(defun non-space? (c) (not (space? c)))
(defun floating? (c) 
  (let ((code (char-code c)))
    (or (= code 46) (>= 57 code 48))))

(defparameter request-line>>
  (with (and>> http-method>> " " (many>> (char>> #'non-space?)) " HTTP/1.1" crlf>>)
	(_fn (method _ uri _ _)
	  (format t "Got the request line (~s ~s)...~%" method uri)
	  (cons (to-string method) (to-string uri)))))

(defun header-char? (c)
  (let ((code (char-code c)))
    (or (= code 45) (>= 122 code 65))))
(defun header-val-char? (c) 
  (> (char-code c) 13))

(defparameter header>>
  (with (and>> (many>> (char>> #'header-char?)) ": " (many>> (char>> #'header-val-char?)) crlf>>)
	(_fn (k _ v _)
	  (format t "Got a header (~s ~s)...~%" k v)
	  (cons (intern (string-upcase (to-string k)) :keyword)
		(to-string v)))))

(defparameter request>>
  (with (and>> request-line>>
	       (many>> header>>))
	(lambda (req headers)
	  (format t "~a~%" req)
	  (format t "~{   ~a~%~}" headers))))

;; (with-input-from-string (s *example*)
;;   (let ((r (rapid s)))
;;     (run! r request>>)))

(defmethod keys ((h hash-table))
  (loop for k being the hash-keys of h collect k))

(defmethod test-server ((port integer) &key (host usocket:*wildcard-host*))
  (let ((server (socket-listen host port :reuse-address t))
	(conns (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready in (wait-for-input (cons server (keys conns)) :ready-only t)
		  do (process-ready ready conns)))
      (flet ((kill-sock! (sock)
	       (loop while (socket-close sock))))
	(loop for c being the hash-keys of conns do (kill-sock! c))
	(kill-sock! server)))))

(defmethod process-ready ((ready stream-server-usocket) (conns hash-table))
  (format t "Got connection...~%")
  (let ((client (socket-accept ready)))
    (setf (gethash client conns)
	  (pause 
	    (run! 
	     (rapid (socket-stream client))
	     request>>)))
    nil))

(defmethod process-ready ((ready stream-usocket) (conns hash-table))
  (format t "Processing client...~%")
  (let ((res (resume (gethash ready conns))))
    (cond ((paused-p res)
	   (format t "Still waiting...~%")
	   (setf (gethash ready conns) res))
	  (t
	   (format t "PARSED!~%~a~%~%" res)
	   (remhash ready conns)))))

;; (defparameter *sock* (usocket:socket-connect "localhost" 5000))
;; (write-string "GET /test HTTP/1.1" (socket-stream *sock*))
;; (write-char #\return (socket-stream *sock*))
;; (write-char #\linefeed (socket-stream *sock*))
;; (force-output (socket-stream *sock*))

;; (write-string "Host: www.example.com" (socket-stream *sock*))
;; (write-char #\return (socket-stream *sock*))
;; (write-char #\linefeed (socket-stream *sock*))
;; (force-output (socket-stream *sock*))

;; (write-string "Content-Type: text/plain" (socket-stream *sock*))
;; (write-char #\return (socket-stream *sock*))
;; (write-char #\linefeed (socket-stream *sock*))
;; (force-output (socket-stream *sock*))

;; (write-char #\return (socket-stream *sock*))
;; (write-char #\linefeed (socket-stream *sock*))
;; (force-output (socket-stream *sock*))
