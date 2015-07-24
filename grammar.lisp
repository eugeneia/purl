;;;; Grammar for URLs

(in-package :purl)

(defun =user ()
  "Parser for user."
  (=string-of (=not (=or (=character #\:)
                         (=character #\@)))))

(defun =password ()
  "Parser for password."
  (=and (=character #\:)
        (=string-of (=not (=character #\@)))))

(defun =credentials ()
  "Parser for user credentials."
  (=prog1 (=list (=maybe (=user))
                 (=maybe (=password)))
          (=character #\@)))

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
	  (=maybe (=string-of (=not (=end-of-input))))))

(defun =common-address ()
  "Parser for common address."
  (=let* ((credentials (=maybe (=credentials)))
          (host (=maybe (=host)))
          (port (=maybe (=port)))
          (path (=maybe (=path)))
          (_ (=end-of-input)))
    (=result (list (first credentials)
                   (second credentials)
                   host
                   port
                   path))))
(defun =scheme ()
  "Parser for URL scheme."
  (=prog1 (=string-of (=or (=satisfies 'alphanumericp)
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
