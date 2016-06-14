;;;; PURL:  Parse and print URLs (ignoring encoding).

(in-package :purl)

(defstruct common-address
  "Common address structure for IP-based protocols."
  (user nil :type (or string null))
  (password nil :type (or string null))
  (host nil :type (or string null))
  (port nil :type (or unsigned-byte null))
  (path nil :type (or string null)))

(defstruct (url (:constructor make-url%)
                (:predicate url-p%))
  "A _url_ is a structured _object_ which represents a _Uniform Resource
  Locator_ (URL)."
  (scheme% nil :type keyword)
  (address% nil :type (or string common-address)))

(defun common-address-string (common-address)
  "Return string for COMMON-ADDRESS."
  (format nil "~@[~a~]~@[:~a~]~@[@~*~]~@[~a~]~@[:~a~]~@[/~a~]"
          #1=(common-address-user common-address)
          #2=(common-address-password common-address)
          (or #1# #2#)
          (common-address-host common-address)
          (common-address-port common-address)
          (common-address-path common-address)))

(defun url-scheme (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-scheme} returns a _keyword_ denoting the scheme part of _url_."
  (url-scheme% (url url)))

(defun url-address (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-address} returns a _string_ denoting the address part of _url_."
  (let ((url (url url)))
    (if (common-address-p #1=(url-address% url))
        (common-address-string #1#)
        #1#)))

(defun url-user (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-user} returns a _string_ denoting the user part of _url_."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-user #1#))))

(defun url-password (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-password} returns a _string_ denoting the password part of
   _url_."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-password #1#))))

(defun url-host (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-host} returns a _string_ denoting the host part of _url_."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-host #1#))))

(defun url-port (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-port} returns a non-negative _integer_ denoting the port part of
   _url_."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-port #1#))))

(defun url-path (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-path} returns a _string_ denoting the path part of _url_."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-path #1#))))

(define-condition malformed-url (error)
  ((message :type string
            :initarg :message
            :initform (error "Must supply MESSAGE.")
            :documentation "Error description."))
  (:report (lambda (error stream)
             (write-string (slot-value error 'message) stream)))
  (:documentation "Condition signaling a malformed URL string."))

(defun url-encode (string)
  "*Arguments and Values:*

   _string_—a _string.

   *Description*:

   {url-encode} encodes _string_ using _Percent-Encoding_¹.

   *See Also:*

   + 1. [Percent-Encoding](http://tools.ietf.org/html/rfc3986#section-2.1)"
  (encode string :test 'unreservedp :www-form nil :encoding :utf-8))

(defun url-decode (string)
  "*Arguments and Values:*

   _string_—a _string.

   *Description*:

   {url-decode} decodes encoded _string_ using _Percent-Encoding_¹.

   *See Also:*

   + 1. [Percent-Encoding](http://tools.ietf.org/html/rfc3986#section-2.1)"
  (decode string :www-form nil :encoding :utf-8))

(defun encode-pathname (path)
  "URL encode physical PATH."
  (make-pathname
   :defaults path
   :device (and #1=(pathname-device path) (url-encode #1#))
   :directory (loop for component in (pathname-directory path)
                 if (keywordp component) collect component
                 else collect (url-encode component))
   :name (and #2=(pathname-name path) (url-encode #2#))
   :type (and #3=(pathname-type path) (url-encode #3#))))

(defun pathname-url-path (path)
  "Return URL path part for PATH."
  (native-namestring
   (encode-pathname (translate-logical-pathname path))))

(defun make-url (scheme &key address user password host port path)
  "*Arguments and Values:*

   _scheme_—a _keyword_ denoting a URL scheme.

   _address_—a _string_ denoting a URL address for _scheme_ or {nil}. The
   default is {nil}.

   _user_, _password_, _host_—_strings_ denoting a user name, password or
   hostname respectively or {nil}. The default is {nil}.

   _path_—a _string_ or a _pathname_.

   _port_—a positive _integer_ denoting a port number or {nil} . The
   default is {nil}.

   *Description*:

   {make-url} returns a fresh _url_ of _scheme_. _Address_ is used as the
   URL's address if supplied. Otherwise the URL's address will use
   _Common Internet Scheme Syntax_¹ and its address is composed of the
   _user_, _password_, _host_, _port_ and _path_ components.

   *See Also:*

   + 1. [Common Internet Scheme Syntax](http://tools.ietf.org/html/rfc1738#section-3.1)"
  (if address
      (make-url% :scheme% scheme :address% address)
      (make-url% :scheme% scheme
                 :address%
                 (make-common-address
                  :host host
                  :user user
                  :password password
                  :port port
                  :path (etypecase path
                          (pathname (native-namestring path))
                          (string path)
                          (null))))))

(defun parse-common-address (url-address)
  "Parse COMMON-ADDRESS structure from URL-ADDRESS."
  (destructuring-bind (user password host port path)
      (or (parse url-address (=common-address))
          (error 'malformed-url
                 :message "Can not parse common address."))
    (make-common-address
     :user user :password password :host host :port port :path path)))

(defun parse-url (string)
  "Return URL structure for STRING."
  (destructuring-bind (&optional scheme common-address-p address)
      (parse string (=url))
    (unless scheme
      (error 'malformed-url :message "Can not parse URL."))
    (make-url% :scheme% (intern (string-upcase scheme) :keyword)
               :address% (if common-address-p
                             (parse-common-address address)
                             address))))

(defun url (urlspec)
  "*Arguments and Values:*

   _urlspec_—a _string_, a _pathname_ or a _url_.

   *Description*:

   {url} returns the _url_ denoted by _urlspec_. When _urlspec_ is a
   string, {url} will attempt to parse _urlspec_ as a URL. When _urlspec_
   is a _pathname_, {url} will return an equivalent _url_ using the
   {:file} scheme.

   *Exceptional Situations:*

   An error of _type_ {malformed-url} is signaled when _urlspec_ is a
   _string_ and can not be parsed as a URL.

   An error of _type_ {type-error} is signaled when _urlspec_ is a
   _pathname_ using a host component which is not a defined logical
   host."
  (etypecase urlspec
    (string (parse-url urlspec))
    (pathname (make-url :file :path (native-namestring urlspec)))
    (url urlspec)))

(defun url-p (object)
  "*Arguments and Values:*

   _object_—an _object_.

   *Description*:

   {url-p} returns _true_ if _object_ is of _type_ {url}; otherwise,
   returns _false_."
  (or (url-p% object)
      (and (stringp object)
           (handler-case (not (null (url object)))
             (malformed-url () nil)))))

(defun url= (url1 url2)
  "*Arguments and Values:*

   _url1_, _url2_—_urls_.

   *Description*:

   {url=} returns _true_ if _url1_ and _url2_ are equal; otherwise,
   returns _false_."
  (let ((url1 (url url1))
        (url2 (url url2)))
    (and (eq (url-scheme url1) (url-scheme url2))
         (equal (url-user url1) (url-user url2))
         (equal (url-password url1) (url-password url2))
         (equal (url-host url1) (url-host url2))
         (equal (url-port url1) (url-port url2))
         (equal (url-path url1) (url-path url2)))))

(defun url-scheme-string (scheme)
  "Return string for SCHEME."
  (string-downcase (symbol-name scheme)))

(defun url-string (url)
  "*Arguments and Values:*

   _url_—a _url_ designator.

   *Description*:

   {url-string} returns a _string_ representation of _url_."
  (let ((url (url url)))
    (if (common-address-p #1=(url-address% url))
        (format nil "~a://~a"
                (url-scheme-string (url-scheme% url))
                (common-address-string #1#))
        (format nil "~a:~a"
                (url-scheme-string (url-scheme% url))
                #1#))))

(defmethod print-object ((url url) stream)
  (print-object (url-string url) stream))
