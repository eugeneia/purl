;;;; Package defition for PURL.

(defpackage purl
  (:documentation
   "Parse and print _Uniform Resource Locators_ as defined in
    [RFC 1738](http://tools.ietf.org/html/rfc1738).

    < URL Designators

     A _url designator_ is a value accepted by the {url} _function_.
     E.g. a _url_, a _string_ or a _pathname_.

    >")
  (:use :cl
        :maxpc
        :maxpc.char
        :maxpc.digit
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
