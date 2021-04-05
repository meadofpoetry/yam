;; -*- lexical-binding: t -*-

(require 'yam-vars)
(require 'yam-core)
(require 'yam-backend-imap)

(defvar yam-main-buffer-name " *yam-main*"
  "Main yam buffer name.")

(define-derived-mode yam-main-mode special-mode "yam:main"
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary)
  (set (make-local-variable 'revert-buffer-function) #'yam--main-view-real))

(defun yam-main-view ()
  "Create a yam main-view or switch to
the existing one."
  (interactive)
  (let ((buf (get-buffer-create yam-main-buffer-name)))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (yam--start #'yam--main-redraw-buffer))))

(defface mu4e-header-key-face
  '((t :inherit message-header-name :bold t))
  "Face for a header key (such as \"Foo\" in \"Subject:\ Foo\")."
  :group 'mu4e-faces)

(defun yam--main-redraw-buffer ()
  (with-current-buffer yam-main-buffer-name
    (let ((inhibit-read-only t)
          (pos       (point)))
      (erase-buffer)
      (insert
       (propertize "yam" 'face '((t :inherit message-header-name :bold t)))
       "\n\n")
      (mapc #'yam--main-insert-profile yam--connections)
      (yam-main-mode)
      (goto-char pos))))

(defun yam--main-insert-profile (connection)
  (let ((mailbox (yam-backend-list-folders connection))
        (config  (yam-backend-config connection)))
    (insert
     "  "
     (propertize (plist-get config :name) 'face 'mu4e-title-face)
     (propertize ":\n" 'face 'mu4e-title-face))
    (yam--main-insert-mailbox connection mailbox "\t")
    (insert "\n\n")))

(defun yam--main-insert-mailbox (conn mailbox padding)
  (dolist (folder mailbox)
    (let ((name     (plist-get folder :folder))
          (children (plist-get folder :children)))
      (when padding
        (insert padding))
      (insert (if children "🗄" "🗀") name)
      (when (null children)
        (when-let ((status (yam-backend-folder-examine conn folder)))
          (pcase-let ((`(,recent . ,all) status))
            (insert
             (propertize (format " [%S/%S]" recent all)
                         'face (if (zerop recent) 'regular 'bold))))))
      (insert "\n")
      (when children
        (yam--main-insert-mailbox conn children (concat padding "\t"))))))
