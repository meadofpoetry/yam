;; -*- lexical-binding: t -*-

(require 'yam-vars)
(require 'yam-backend-imap)

(defvar yam--connections nil
  "List of currently opened connections
for each of the profiles defined in
 `yam-profiles'.")

(defun yam--start (&optional func)
  (unless yam--connections
    (yam--init-backend-list))
  ;; Ensure connections are alive
  (mapc #'yam-backend-ensure-alive yam--connections)
  (when (functionp func)
    (funcall func)))

(defun yam--stop ()
  (when yam--connections
    (mapc #'yam-backend-stop yam--connections)))

(defun yam--init-backend-list ()
  "Initialize backend list, if it doesn't exist.
Otherwise reconnect backends which died."
  (when (null yam--connections)
    (setq yam--connections
          (mapcar #'yam--backend-connect yam-profiles))))

(defun yam--backend-connect (config)
  "A `factory' function, that creates a backend 
according with provided config."
  (let* ((backend-type (plist-get config :type))
         (backend (make-instance 'yam-backend-imap ;; So far it's imap only
                                 :config config)))
    (if (yam-backend-start backend)
        backend
      (error (format "Yam failed to create backend for %s"
                     (plist-get config :name))))))

(provide 'yam-core)
