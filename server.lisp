(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer (coerce (list c1 c2) 'string)
			     :radix 16
			     :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#\% (cons (http-char (cadr lst) (caddr lst))
			    (f (cdddr lst))))
		 (#\+ (cons #\Space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let* ((i1 (position #\= s))
	 (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\Space s)) ;; skip ' /'
		      (position #\Space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x)
	      (parse-params (subseq url (1+ x))))
	(cons url nil))))

(defun get-header (stream)
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
			    (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

(ql:quickload :usocket)

(defun serve (port request-handler)
  (let ((socket (usocket:socket-listen "127.0.0.1" port)))
    (unwind-protect
	 (loop
	   (with-open-stream (stream (usocket:socket-stream
				      (usocket:socket-accept socket)))
	     (let* ((url (parse-url (read-line stream)))
		    (path (car url))
		    (header (get-header stream))
		    (params (append (cdr url)
				    (get-content-params stream header)))
		    (*standard-output* stream))
	       (funcall request-handler path header params)
	       (force-output stream))))
      (usocket:socket-close socket))))

(defun serve-better (port request-handler)
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (loop
      (with-open-stream (stream (usocket:socket-stream
				 (usocket:socket-accept socket)))
	(let* ((url (parse-url (read-line stream)))
	       (path (car url))
	       (header (get-header stream))
	       (params (append (cdr url)
			       (get-content-params stream header)))
	       (*standard-output* stream))
	  (funcall request-handler path header params)
	  (force-output stream))))))

(defun hello-request-handler (path header params)
  (let ((msg (if (equal path "greeting")
		 (let ((name (assoc 'name params)))
		   (if (not name)
		       "<html><body>
<form>What is your name?<input name='name' /></form>
</body></html>"
		       (format nil "<html><body>
Nice to meet you, ~a
</body></html>"
			       (cdr name))))
		 "Sorry... I don't know that page.")))
    (format t "HTTP/1.1 200 OK~% Content-Length: ~a~%~%~a"
	    (length msg)
	    msg)))
