;; -*- lexical-binding: t -*-

(require 'rfc3501)

(defclass yam-backend-imap (yam-backend)
  ((timer
    :documentation "IMAP ping timer"
    :accessor yam--backend-imap-timer)
   (conn
    :documentation "IMAP connection"
    :accessor yam--backend-imap-conn)
   (folder-examine-cache
    :accessor yam--backend-imap-folder-examine-cache)))

(defun yam--backend-imap-drop-caches (conn)
  (setf (yam--backend-imap-folder-examine-cache conn)
        (make-hash-table :test #'equal)))

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
      (yam--backend-imap-drop-caches conn)
      (setf (yam--backend-imap-conn conn)
            imap-conn
            (yam--backend-imap-timer conn)
            (run-with-timer 300 300
                            (lambda ()
                              (pcase (imap-command-noop imap-conn)
                                (`(ok . ,data)
                                 (when data
                                   (yam--backend-imap-drop-caches conn)))))))
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

(cl-defmethod yam-backend-ensure-alive ((conn yam-backend-imap))
  (let ((imap-conn (yam--backend-imap-conn conn))
        (timer     (yam--backend-imap-timer conn)))
    (unless (imap-connection-alive-p imap-conn)
      (message "[yam] closing connection for %s"
               (plist-get (yam--backend-config conn) :mail-address))
      (cancel-timer timer)
      (imap-connection-close imap-conn)
      (yam-backend-start conn))))

(cl-defmethod yam-backend-stop ((conn yam-backend-imap))
  (let ((imap-conn (yam--backend-imap-conn conn))
        (timer     (yam--backend-imap-timer conn))
        (address   (plist-get (yam--backend-config conn) :mail-address)))
    (when (imap-connection-alive-p imap-conn)
      (message "[yam] closing connection for %s" address)
      (cancel-timer timer)
      (imap-command-logout imap-conn)
      (imap-connection-close imap-conn))))

;; TODO maybe cache
;; TODO translate flags to keys
(cl-defmethod yam-backend-list-folders ((conn yam-backend-imap))
  (let ((imap-conn (yam--backend-imap-conn conn)))
    (message "[yam] list folders")
    ;; TODO maybe use command-lsub instead?
    (pcase-let ((`(ok . ,list) (imap-command-list imap-conn nil "*")))
      (let ((root (list :children nil))
            (flat (mapcar (pcase-lambda (`(LIST ,flags ,delim ,path))
                            (list :folder   (split-string
                                             (imap--base64-decode path)
                                             delim)
                                  :raw-path (format "\"%s\"" path)
                                  :delim    delim
                                  :flags    flags))
                          list)))
        ;; Collect toplevel folders first
        (cl-sort flat (lambda (l r)
                        (< (length (plist-get l :folder))
                           (length (plist-get r :folder)))))
        ;; Construct a tree, where all subfolders
        ;; are to be found under the :children key
        ;; of their parent folder
        (dolist (el flat)
          (let ((path     (plist-get el :folder))
                (children (plist-member root :children)))
            (while (length> path 1)
              (let ((parent (seq-find (lambda (p)
                                        (string= (plist-get p :folder)
                                                 (car path)))
                                      (plist-get children :children))))
                (when (not (plist-member parent :children))
                  (plist-put parent :children nil))
                (setq children (plist-member parent :children)
                      path (cdr path))))
            (push (plist-put el :folder (car path))
                  (plist-get children :children))))
        (plist-get root :children)))))

(cl-defmethod yam-backend-folder-examine ((conn yam-backend-imap) folder)
  (let ((raw-path  (plist-get folder :raw-path))
        (imap-conn (yam--backend-imap-conn conn))
        (cache     (yam--backend-imap-folder-examine-cache conn)))
    (if-let ((cached (gethash raw-path cache)))
        cached
      (pcase-let ((`(ok . ,data)
                   (imap-command-examine imap-conn raw-path)))
        (let (recent exists)
          (dolist (el data)
            (print el)
            (pcase el
              (`(,n EXISTS) (setq exists n))
              (`(,n RECENT) (setq recent n))))
          (when (and recent exists)
            (let ((res (cons recent exists)))
              (puthash raw-path res cache)
              res)))))))

(provide 'yam-backend-imap)
