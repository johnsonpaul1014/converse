(defpackage :port
  (:export http-get)
  (:use common-lisp))

(in-package :port)

;;; Utilities

(defun http-send-line (stream line)
  "Send a line of text to the HTTP stream, terminating it with CR+LF."
  (princ line stream)
  (princ (code-char 13) stream)  ;; carriage-return
  (princ (code-char 10) stream)) ;; linefeed
;;; Server

(defun http-0.9-server (port root)
  "Run an HTTP/0.9 server on `port'. `root' is the pathname to the
directory where the HTML pages are stored."
  (let ((server (socket:socket-server port)))
    (format t "> Started server on port ~d~%" port)
    (unwind-protect
        (loop
          (let ((socket (socket-accept server)))
            (unwind-protect
                (process-request socket
                                 (read-request socket)
                                 root)
              ;; Close connection when done
              (close socket))))
      
      ;; Close server before exiting
      (socket-server-close server))))

(defun read-request (socket)
  "Read an HTTP/0.9 request from `socket' and determine the
corresponding filename. An HTTP/0.9 request has the form:

GET /filename HTTP/0.9

Returns the filename, or NIL if the request is incorrect."
  
  (let ((request (read-line socket nil nil)))
    (when request
      (let ((p1 (position #\Space request))
            (p2 (position #\Space request :from-end t)))
        (when (and p1 p2)
          (subseq request (1+ p1) p2))))))

(defun process-request (socket filename root)
  (format t "> Received request from host ~a~%"
          (socket-host/port socket))
  ;; discard empty line
  (read-line socket)
  (if filename
      ;; Correct request, serve file
      (serve-file socket
                  (concatenate 'string root filename))
    ;; Incorrect request, return error
    (http-send-line socket "HTTP/0.9 400 Invalid HTTP Request."))
  
  ;; Make sure the client sees the output - not really
  ;; necessary since we close the socket right after this.
  (force-output socket))

(defun serve-file (socket pathname)
  "Write the contents of the file `pathname' to `socket'."
  ;; Does file exist?
  (if (probe-file pathname)
      
      ;; Yes, write it out to the socket
      (with-open-file (in pathname)
        (format t "> Serving file ~a...~%" pathname)
        (loop
          (let ((line (read-line in nil nil)))
            (unless line (return))
            (http-send-line socket line))))
    
    ;; No, return error
    (format socket "HTTP/0.9 401 Not found.~%")))

;;; Client
(defun http-get (server port path)
  "Generic http GET.  Just reads bytes one at a time from host"
  
	;; Stolen from http://clisp.cons.org/impnotes.html#sose
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port server :EXTERNAL-FORMAT :DOS))
    (FORMAT socket "GET ~A HTTP/1.0~%Host: ~A~2%" path server)
		(SETF (STREAM-ELEMENT-TYPE socket) '(UNSIGNED-BYTE 8))
		(let ((http-response ""))
			(LOOP :for current-byte = (READ-BYTE socket)
			  :until (or (null current-byte) (equal (socket:socket-status socket) :append))
				:do (setf http-response (concatenate 'string http-response (string (code-char current-byte)))))
			http-response)))

(defun http-get-fast (server port path)
  "Send a request for file `path' to an HTTP/0.9 server on host
`server', port number `port'. Return the contents of the file.
 Assumes a content-length exists for faster transfer"
  
	;; Stolen from http://clisp.cons.org/impnotes.html#sose
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port server :EXTERNAL-FORMAT :DOS))
    (FORMAT socket "GET ~A HTTP/1.0~%Host: ~A~2%" path server)
    (LOOP :with content-length :for line = (READ-LINE socket nil nil)
      :until (or content-length (equal line nil) (equal (socket:socket-status socket) :append)) 
			:do (let ((match-values (multiple-value-list (cl-ppcre:scan-to-strings "Content-Length: (\\d+)" line))))
						(if (not (equal '(nil) match-values))
							(setf content-length (parse-integer (aref (cadr match-values) 0)))))
      ;; this will not work if the server does not supply the content-length header
      :finally (RETURN 
				(LET ((data (MAKE-ARRAY content-length :element-type '(UNSIGNED-BYTE 8))))
					;; switch to binary i/o on socket
          (SETF (STREAM-ELEMENT-TYPE socket) '(UNSIGNED-BYTE 8))
          ;; read the whole file in one system call
          (EXT:READ-BYTE-SEQUENCE data socket)
					(let ((http-response ""))
						(dotimes (i content-length t)
							(setf http-response (concatenate 'string http-response (string (code-char (aref data i))))))
						http-response))))))