;;;; Tests for PURL.

(defpackage purl-test
  (:use :cl :purl)
  (:export :run-tests))

(in-package :purl-test)

(defun test-parsers ()
  (loop for (string result)
     in '(("scheme:body"
	   #.(make-url :scheme
	      :address "body"))
	  ("scheme://host"
	   #.(make-url :scheme
              :host "host"))
	  ("scheme://:password@host"
	   #.(make-url :scheme
	      :host "host" :password "password"))
	  ("scheme://user@host"
	   #.(make-url :scheme
	      :host "host" :user "user"))
	  ("scheme://user:password@host"
	   #.(make-url :scheme
	      :host "host" :user "user" :password "password"))
	  ("scheme://host:42"
	   #.(make-url :scheme
	      :host "host" :port 42))
	  ("scheme://host/path"
	   #.(make-url :scheme
	      :host "host" :path "path"))
	  ("scheme://host:42/path"
	   #.(make-url :scheme
	      :host "host" :port 42 :path "path")))
     do (unless (url= (url string) result)
	  (error "Parser: ~a is not equal to ~a." string result))))

(defun run-tests ()
  "Run PURL tests."
  (test-parsers))
