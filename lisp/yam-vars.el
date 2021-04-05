;; -*- lexical-binding: t -*-

(defgroup yam nil
  "yam — yet another mail client 
for Emacs."
  :group 'mail)

(defcustom yam-profiles nil
  "List of user profiles"
  :group 'yam
  :type '(repeat (plist :value-type t)))

(provide 'yam-vars)
