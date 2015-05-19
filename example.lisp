(in-package :cl-lazy-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Example
(defparameter *examples*
  "GET /index.html HTTP/1.1
Host: www.example.com

"
  "POST /index.html HTTP/1.1
Host: www.example.com
Content-Length: 11

a=1&b=2&c=3"

  "GET /index.html?a=1&b=2 HTTP/1.1
Host: www.example.com

"
  "POST /index.html?foo=123&bar=456 HTTP/1.1
Host: www.example.com
Content-Length: 11

a=1&b=2&c=3")

;;;; Predicates and utility
(defun to-string (seq)
  (coerce seq 'string))

(defun to-key (seq)
  (intern (string-upcase (to-string seq)) :keyword))

(defun header-char? (c)
  (let ((code (char-code c)))
    (or (= code 45) (>= 122 code 65))))

(defun snd>> (parser) (with parser (_fn (_ p) p)))

;;;; Parsers
(defparameter crlf>> (and>> #\return #\linefeed))

(defparameter param>>
  (and>> (many>> (none-of>> "=& ")) "=" (many>> (none-of>> "& ")) (optionally>> #\&)))

(defparameter request-line>>
  (and>> (or>> "GET" "POST" "PUT" "DELETE") " " 
	  (many>> (none-of>> "? "))
	  (optionally>> (snd>> (and>> "?" (many>> param>>))))
	  " HTTP/1.1" crlf>>))

(defparameter header>>
  (and>> (many>> (char>> #'header-char?)) ": " (many>> (none-of>> '(#\return #\linefeed))) crlf>>))

(defparameter request-stateless>>
  (and>> request-line>>
	 (many>> header>>)
	 crlf>>
	 (optionally>> (many>> param>>))))

(defun request>> ()
  (let ((method)
	(uri)
	(headers)
	(parameters)
	(content-length 0)
	(content-type))
    (let ((par>>
	   (with param>>
		 (_fn (k _ v _)
		   (push (cons (to-key k) (to-string v)) parameters)))))
      (with (and>> 
	     ;;; request-line
	     (with (or>> "GET" "POST" "PUT" "DELETE")
		   (lambda (&rest m) 
		     (format t "Setting method!~%")
		     (setf method (to-key m))))
	     " " (many>> (none-of>> "? ")) (optionally>> (and>> "?" (many>> par>>)))
	     " HTTP/1.1" crlf>>

	     ;;; headers
	     (many>> 
	      (with header>>
		    (_fn (key _ val _)
		      (let ((k (to-key key))
			    (v (to-string val)))
			(case k
			  (:content-length (setf content-length (parse-integer v)))
			  (:content-type (setf content-type v))
			  (t (push (cons k v) headers))))
		      nil)))

	     ;;; body
	     (if>> (and (eq method :post) (> content-length 0))
		   (and>> crlf>> (many>> par>>))))
	    (_fn (&rest _)
	      (list :method method :uri uri :headers headers :parameters (reverse parameters)))))))

;;;; A test server
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
	   (let ((stream (socket-stream ready)))
	     (flet ((write-lns (&rest strings)
		      (loop for s in strings
			 do (write-string s stream)
			 do (write-char #\return stream)
			 do (write-char #\linefeed stream))))
	       (ignore-errors
		 (write-lns 
		  "HTTP/1.1 200 Ok"
		  "Content-Type: text/plain; charset=utf-8"
		  "Content-Length: 12"
		  ""
		  "Hello There!"))
	       (ignore-errors
		 (socket-close ready))))
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
