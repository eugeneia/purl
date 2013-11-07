;;;; PURL:  Parse and print URLs (ignoring encoding).

(in-package :purl)

(defstruct common-address
  "Common address structure for IP-based protocols."
  (user nil :type (or string symbol))
  (password nil :type (or string symbol))
  (host nil :type (or string symbol))
  (port nil :type (or unsigned-integer symbol))
  (path nil :type (or string symbol)))

(defstruct (url (:constructor make-url%)
		(:predicate url-p%))
  "URL type."
  (scheme% nil :type symbol)
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
  "Get scheme for URL."
  (url-scheme% (url url)))

(defun url-address (url)
  "Get address for URL."
  (let ((url (url url)))
    (if (common-address-p #1=(url-address% url))
        (common-address-string #1#)
        #1#)))

(defun url-user (url)
  "Get user for URL."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-user #1#))))

(defun url-password (url)
  "Get password for URL."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-password #1#))))

(defun url-host (url)
  "Get host for URL."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-host #1#))))

(defun url-port (url)
  "Get port for URL."
  (let ((url (url url)))
    (when (common-address-p #1=(url-address% url))
      (common-address-port #1#))))

(defun url-path (url)
  "Get path for URL."
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

(defun make-url (scheme &key address user password host port path)
  "Make URL for SCHEME and ADDRESS or SCHEME and USER, PASSWORD, HOST,
  PORT and PATH."
  (if address
      (make-url% :scheme scheme :address% address)
      (make-url% :scheme scheme
                 :address% (make-common-address
                            :host host
                            :user user
                            :password password
                            :port port
                            :path path))))

(defun parse-common-address (url-address)
  "Parse COMMON-ADDRESS structure from URL-ADDRESS."
  (destructuring-bind (user password host port path)
      (or (run (=common-address) url-address)
          (error 'malformed-url
                 :message "Can not parse common address."))
    (make-common-address
     :user user
     :password password
     :host (unless (= 0 (length host)) host)
     :port port
     :path (unless (= 0 (length path)) path))))

(defun parse-url (string)
  "Return URL structure for STRING."
  (destructuring-bind (&optional scheme common-address-p address)
      (run (=url) string)
    (unless scheme
      (error 'malformed-url :message "Can not parse URL."))
    (make-url% :scheme% (intern (string-upcase scheme) :keyword)
	       :address% (if common-address-p
			     (parse-common-address address)
			     address))))

(defun url (url)
  "If URL is a string return parsed URL structure. Otherwise return URL
as is."
  (etypecase url
    (string (parse-url url))
    (url url)))

(defun url-p (thing)
  "Predicate to test if THING is a valid URL."
  (or (url-p% thing)
      (and (stringp thing)
	   (handler-case (not (null (url thing)))
	     (malformed-url () nil)))))

(defun url= (url-x url-y)
  "Test URL-X and URL-Y for equality."
  (let ((url-x (url url-x))
        (url-y (url url-y)))
    (and (eq (url-scheme% url-x) (url-scheme% url-y))
         (equal (url-user url-x) (url-user url-y))
         (equal (url-password url-x) (url-password url-y))
         (equal (url-host url-x) (url-host url-y))
         (equal (url-port url-x) (url-port url-y))
         (equal (url-path url-x) (url-path url-y)))))

(defun url-scheme-string (scheme)
  "Return string for SCHEME."
  (string-downcase (symbol-name scheme)))

(defun url-string (url)
  "Return string for URL."
  (let ((url (url url)))
    (if (common-address-p #1=(url-address% url))
        (format nil "~a://~a"
                (url-scheme-string (url-scheme% url))
                (common-address-string #1#))
        (format nil "~a:~a"
                (url-scheme-string (url-scheme% url))
                #1#))))

(defmethod print-object ((url url) stream)
  (format stream "~a" (url-string url)))

(defun reserved-p (char)
  "Predicate to test if CHAR is reserved."
  (not (or (alphanumericp char)
	   (member char '(#\$ #\- #\_ #\. #\+ #\!
			  #\* #\' #\( #\) #\,)))))

(defun url-encode (string)
  "Encode STRING to be URL safe."
  (%-encode-string string #'reserved-p))

(defun url-decode (string)
  "Decode URL encoded STRING."
  (%-decode-string string))
