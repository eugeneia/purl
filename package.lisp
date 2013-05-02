;;;; Package defition for PURL.

(defpackage purl
  (:documentation "Parse and print URLs.")
  (:use :cl
	:smug
	:smug.characters
	:smug.numerals
	:percent-encoding)
  (:export :make-url
	   :url
	   :url-p
	   :url-scheme
	   :url-address
	   :url-user
	   :url-password
	   :url-host
	   :url-port
	   :url-path
	   :url=
	   :url-string
	   :url-encode
	   :url-decode))
