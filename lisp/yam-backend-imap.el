;; -*- lexical-binding: t -*-

(require 'rfc3501)

(defclass yam-backend-imap (yam-backend)
  ((timer
    :documentation "IMAP ping timer"
    :accessor yam--backend-imap-timer)
   (conn
    :documentation "IMAP connection"
    :accessor yam--backend-imap-conn)))

(cl-defmethod yam-backend-start ((conn yam-backend-imap))
  (let* ((config    (yam-backend-config conn))
         (name      (plist-get config :name))
         (address   (plist-get config :mail-address))
         (server    (plist-get config :imap-server))
         (imap-conn (imap-connect server)))
    (when (memq (process-status (imap-connection-proc imap-conn))
                '(open run))
      (message "[yam] imap connection for %s successfully started"
               address)
      (setf (yam--backend-imap-conn conn)
            imap-conn
            (yam--backend-imap-timer conn)
            (run-with-timer 300 300
                            (lambda ()
                              (imap-command-noop imap-conn))))
      ;; Get capabilities
      (imap-command-capability imap-conn)
      ;; Auth
      (if (eq (plist-get config :auth-method)
              'xoauth2)
          (let ((auth-fun (plist-get config :auth-function)))
            (imap-command-xoauth2 imap-conn
                                  address
                                  (funcall auth-fun address)))
        (let ((pass (read-passwd (format "Password for %s: " address))))
          (imap-command-login imap-conn
                              address
                              pass)))
      t)))

(cl-defmethod yam-backend-stop ((conn yam-backend-imap))
  (let ((imap-conn (yam--backend-imap-conn conn))
        (timer     (yam--backend-imap-timer conn))
        (address   (plist-get (yam--backend-config conn) :mail-address)))
    (when (imap-connection-alive-p imap-conn)
      (message "[yam] closing connection for %s" address)
      (cancel-timer timer)
      (imap-command-logout imap-conn)
      (imap-connection-close imap-conn))))
