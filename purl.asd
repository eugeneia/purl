;;;; System definition for PURL.

(defpackage purl-asd
  (:documentation
   "System definition for PURL.")
  (:use :cl :asdf))

(in-package :purl-asd)

(defsystem purl
  :description "Parse and print URLs as described in RFC 1739."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "package")
	       (:file "grammar"
		      :depends-on ("package"))
	       (:file "purl"
		      :depends-on ("package" "grammar")))
  :depends-on ("mpc" "percent-encoding"))
