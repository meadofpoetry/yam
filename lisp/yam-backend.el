;; -*- lexical-binding: t -*-

(defclass yam-backend ()
  ((config
    :documentation "User config"
    :initarg :config
    :reader yam-backend-config
    :accessor yam--backend-config)))

(defgeneric yam-backend-start (backend)
  "Initialized connection")

(defgeneric yam-backend-stop (backend)
  "Closes the connection and does all 
necessary cleanup.")

(defvar yam-config nil
  "Yam user configuration.")
      
(defun yam-connect (config)
  "Create a backend according with provided config."
  (let* ((backend-type (plist-get config :type))
         (backend (make-instance 'yam-backend-imap ;; So far it's imap only
                                 :config config)))
    (when (yam-backend-start backend)
      backend)))
