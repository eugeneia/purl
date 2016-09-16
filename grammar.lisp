;;;; Grammar for URLs

(in-package :purl)

(defun =user ()
  "Parser for user."
  (=subseq (%any (?not (%or (?char #\:) (?char #\@))))))

(defun =password ()
  "Parser for password."
  (=destructure (_ password)
      (=list (?char #\:) (=subseq (%any (?not (?char #\@)))))))

(defun =credentials ()
  "Parser for user credentials."
  (=destructure (user password _)
      (=list (=user) (%maybe (=password)) (?char #\@))
    (list user password)))

(defun =host ()
  "Parser for host."
  (=subseq (%some (?not (%or (?char #\:) (?char #\/))))))

(defun =port ()
  "Parser for port."
  (=destructure (_ port) (=list (?char #\:) (=natural-number))))

(defun =path ()
  "Parser for path."
  (=destructure (_ path) (=list (?char #\/) (=subseq (%some (?not (?end)))))))

(defun =common-address ()
  "Parser for common address."
  (=destructure (credentials host port path _)
      (=list (%maybe (=credentials))
             (%maybe (=host))
             (%maybe (=port))
             (%maybe (=path))
             (?end))
    (list (first credentials) (second credentials) host port path)))

(defun =scheme ()
  "Parser for URL scheme."
  (=destructure (scheme _)
      (=list (=subseq (%any (%or (?satisfies 'alphanumericp)
                                 (?test ('member '(#\+ #\. #\-))))))
             (?char #\:))))

(defun =common-address-p ()
  "Parser for URI address type."
  (=transform (?string "//") (constantly t)))

(defun =url ()
  "Parser for URL."
  (=list (=scheme)
         (%maybe (=common-address-p))
         (=subseq (%any (?not (?end))))))
