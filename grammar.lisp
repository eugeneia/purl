;;;; Grammar for URLs

(in-package :purl)

(defun =user ()
  "Parser for user."
  (let ((terminator (=or (=character #\:)
			 (=character #\@))))
    (=prog1 (=string-of (=not terminator))
	    terminator)))

(defun =password ()
  "Parser for password."
  (let ((terminator (=character #\@)))
    (=prog1 (=string-of (=not terminator))
	    terminator)))

(defun =host ()
  "Parser for host."
  (=string-of (=not (=or (=character #\:)
			 (=character #\/)
			 (=end-of-input)))))

(defun =port ()
  "Parser for port."
  (=prog2 (=character #\:)
	  (=natural-number)))

(defun =path ()
  "Parser for path."
  (=prog2 (=character #\/)
	  (=string-of (=not (=end-of-input)))))

(defun =common-address ()
  "Parser for common address."
  (=prog1 (=list (=maybe (=user))
		 (=maybe (=password))
		 (=host)
		 (=maybe (=port))
		 (=maybe (=path)))
	  (=end-of-input)))

(defun =scheme ()
  "Parser for URL scheme."
  (=prog1 (=string-of (=or (=satisfies #'alphanumericp)
			   (=one-of '(#\+ #\. #\-))))
	  (=character #\:)))

(defun =common-address-p ()
  "Parser for URI address type."
  (=and (=string "//")
	(=result t)))

(defun =url ()
  "Parser for URL."
  (=list (=scheme)
	 (=maybe (=common-address-p))
	 (=string-of (=not (=end-of-input)))))
