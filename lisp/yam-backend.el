;; -*- lexical-binding: t -*-

(defclass yam-backend ()
  ((config
    :documentation "User config"
    :initarg :config
    :reader yam-backend-config
    :accessor yam--backend-config)))

(defgeneric yam-backend-start (backend)
  "Initialized connection")

(defgeneric yam-backend-ensure-alive (backend)
  "Ensure if backend is alive")

(defgeneric yam-backend-stop (backend)
  "Close the connection and do all necessary cleanup.")

(defgeneric yam-backend-list-folders (backend)
  "Return the list of mail folders for current user.")

(defgeneric yam-backend-folder-examine (backend folder)
  "Return (`RECENT-LETTERS' . `ALL-LETTERS')")

(provide 'yam-backend)
