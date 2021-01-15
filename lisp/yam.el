;; -*- lexical-binding: t -*-

(require 'rfc3501)

;;;###autoload
(defun yam ()
  "Yam main buffer"
  (interactive)
  (error "Not implemented"))

;; Access token

(defun get-access-token-by-user (user)
  (let ((objects
         (dbus-get-all-managed-objects :session "org.gnome.OnlineAccounts" "/"))
        thepath)
    (setq thepath
          (car
           (seq-find
            (lambda (obj)
              (let ((mail-interface
                     (assoc "org.gnome.OnlineAccounts.Mail" obj)))
                (equal user (cdr (assoc "ImapUserName" mail-interface)))))
            objects)))
    (if thepath
        (dbus-call-method
         :session
         "org.gnome.OnlineAccounts"
         thepath
         "org.gnome.OnlineAccounts.OAuth2Based"
         "GetAccessToken")
      (message "get-access-token-by-user failed: %s" user)
      nil)))
