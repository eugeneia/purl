;;;; Package defition for PURL.

(defpackage purl
  (:documentation
   "Parse and print _Uniform Resource Locators_ as defined in
    [RFC 1738](http://tools.ietf.org/html/rfc1738).

    < URL designator

     A _url designator_ is considered to be a value accepted by the {url}
     _function_.

    >")
  (:use :cl
	:mpc
	:mpc.characters
	:mpc.numerals
	:percent-encoding
        :uiop)
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
