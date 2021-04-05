;; -*- lexical-binding: t -*-

(require 'utf7)

(cl-defstruct imap-connection
  "Represents an opened connection to
some IMAP server"
  (server nil :type string)
  (port   nil :type integer)
  (type   nil :type symbol)
  (caps   nil :type list)
;;  (state  :type symbol)
  (buffer nil :type buffer)
  (proc   nil :type process)
  (greeting nil :type string)
  (command-id nil :type integer))

(cl-defun imap-connect (server &optional (type 'ssl) port)
  (let* ((port (or port
                  (if (memq type '(ssl tls)) 993 143)))
         (buf  (generate-new-buffer "*imap*"))
         (conn (open-network-stream
                "*imap*" buf server port
                :type type
                :warn-unless-encrypted t
                :return-list t
                :capability-command "1 CAPABILITY\r\n"
                :always-query-capabilities t
                :end-of-command "\r\n"
                :success " OK "
                :starttls-function (lambda (capabilities)
		                     (when (string-match-p "STARTTLS" capabilities)
		                       "1 STARTTLS\r\n")))))
    (let* ((proc     (car conn))
           (plist    (cdr conn))
           (greeting (plist-get plist :greeting))
           (caps     (mapcar #'upcase
                             (cddr (split-string (plist-get plist :capabilities)))))
           (real-type (plist-get plist :type)))

      (when (or (null proc)
                (not (memq (process-status proc) '(open run))))
        (error "Failed to start an imap connection process"))

      (when (eq (process-type proc) 'network)
        ;; Use TCP-keepalive so that connections that pass through a NAT
        ;; router don't hang when left idle.
        (set-network-process-option proc :keepalive t))

      (set-process-query-on-exit-flag proc nil)

      (when (not (string-match-p "[*.] \\(OK\\|PREAUTH\\)" greeting))
        (error "Server refused the connection (%s)" greeting))

      ;; TODO deal with PREAUTH properly
      (when (string-match-p "[*.] PREAUTH" greeting)
        (error "This IMAP implementation don't support preauth yet"))

      (make-imap-connection
       :server server
       :port port
       :type real-type
       ;;:state 'not-authenticated
       :caps caps
       :buffer buf
       :proc proc
       :greeting greeting
       :command-id 2))))

(defun imap-connection-close (conn)
  (let ((proc (imap-connection-proc conn))
        (buf  (imap-connection-buffer conn)))
    (delete-process proc)
    (kill-buffer buf)))

(defun imap-connection-alive-p (conn)
  (memq (process-status (imap-connection-proc conn))
        '(open run)))

;; Generic commands

(defun imap-command-capability (conn)
  "The CAPABILITY command requests a listing of capabilities that the
server supports. Updates the connection's caps field. Returns the response
code, resets connection capabilities."
  (imap-connection-send conn
                        :message "CAPABILITY"
                        :get-response (lambda (resp)
                                        (when-let ((val (assq 'CAPABILITY resp)))
                                          (setf (imap-connection-caps conn)
                                                (cdr val))))))

(defun imap-command-noop (conn)
  "Command wich `does nothing' but may be used to
fetch some useful data or reset inactivity timer.
Returns a cons pair of (response-code . data)."
  (let (resp-data)
    (cons (imap-connection-send conn
                                :message "NOOP"
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

(defun imap-command-logout (conn)
  "Issue LOGOUT call. Returns response-code."
  (imap-connection-send conn :message "LOGOUT"))

;; Not authentificated state

(defun imap-command-starttls (conn)
  "Try to upgrade the connection to STARTTLS. Returns
the response-code."
  (imap-connection-send conn :message "STARTTLS"))

(defun imap-command-xoauth2 (conn user token)
  "Authericate using provided XOAUTH2 token. Returns
the response code."
  (let ((data
         (base64-encode-string (format "user=%s\001auth=Bearer %s\001\001" user token))))
    (imap-connection-send conn
                          :message (format "AUTHENTICATE XOAUTH2 %s" data)
                          :provide-reply (lambda (s)
                                           (let ((resp (base64-decode-string (string-remove-prefix "+ " s))))
                                             (message (format "Got resp: %s" resp))
                                             "")))))

(defun imap-command-login (conn user password)
  "Send login command with plaintext username and 
password. Returns the response code."
  (imap-connection-send conn :message (format "LOGIN %s %s" user password)))

;; Authenicated state

(defun imap-command-select (conn mailbox-name)
  "Select a mailbox with mailbox-name. If succeed,
shift connection to Selected state. Returns a cons
pair of (response-code . data)."
  (let (resp-data)
    (cons (imap-connection-send conn
                                :message (format "SELECT %s" mailbox-name)
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

(defun imap-command-examine (conn mailbox-name)
  "Identical to select, but selected mailbox is identified as read-only.
In particular messages wont lose the \Recent flag. Returns a cons
paid of (response-code . data)."
  (let (resp-data)
    (cons (imap-connection-send conn
                                :message (format "EXAMINE %s" mailbox-name)
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

(defun imap-command-create (conn mailbox-path)
  "Ask server to create a new mailbox directory, e.g.
  (imap-command-create ... \"foo\") will create a mailbox foo,
  (imap-command-create ... \"foo/bar\") will create a directory foo
containing mailbox bar. Returns response-code."
  (imap-connection-send conn
                        :message (format "CREATE %s" mailbox-path)))

(defun imap-command-delete (conn mailbox-path)
  "Similar to imap-command-create, but for deletion.
Returns response-code."
  (imap-connection-send conn
                        :message (format "DELETE %s" mailbox-path)))

(defun imap-command-rename (conn old-mailbox-path new-mailbox-path)
  "Renames `old-mailbox-path' to `new-mailbox-path',
returns response-code."
  (imap-connection-send conn
                        :message (format "RENAME %s %s"
                                         old-mailbox-path
                                         new-mailbox-path)))

;; TODO Subscribe/Unsubscribe

(defun imap-command-list (conn reference mailbox-name)
  "TODO fill in

     Reference     Mailbox Name  Interpretation
     ------------  ------------  --------------
     ~smith/Mail/  foo.*         ~smith/Mail/foo.*
     archive/      %             archive/%
     #news.        comp.mail.*   #news.comp.mail.*
     ~smith/Mail/  /usr/doc/foo  /usr/doc/foo
     archive/      ~fred/Mail/*  ~fred/Mail/*

"
  (let (resp-data
        (reference (if (or (null reference)
                           (string-empty-p reference))
                       "\"\""
                     reference))
        (mailbox-name (if (or (null mailbox-name)
                              (string-empty-p mailbox-name))
                          "\"\""
                        mailbox-name)))
    (cons (imap-connection-send conn
                                :message (format "LIST %s %s" reference mailbox-name)
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

(defun imap-command-lsub (conn reference mailbox-name)
  "TODO"
  (let (resp-data
        (reference (if (or (null reference)
                           (string-empty-p reference))
                       "\"\""
                     reference))
        (mailbox-name (if (or (null mailbox-name)
                              (string-empty-p mailbox-name))
                          "\"\""
                        mailbox-name)))
    (cons (imap-connection-send conn
                                :message (format "LSUB %s %s" reference mailbox-name)
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

(defconst imap--status-data-items
  '(MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN)
  "The currently defined status data items that 
can be requested with `imap-command-status'")

(defun imap-command-status (conn mailbox-name data-items)
  (when (or (not (consp data-items))
            (cl-notevery (lambda (el)
                           (memq el imap--status-data-items))
                         data-items))
    (error "Unknown data items provided in STATUS, possible values %S"
           imap--status-data-items))
  (let (resp-data
        (mailbox-name (if (or (null mailbox-name)
                              (string-empty-p mailbox-name))
                          "\"\""
                        mailbox-name)))
    (cons (imap-connection-send conn
                                :message (format "STATUS %s %S" mailbox-name data-items)
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

;; TODO implement date
(defun imap-command-append (conn mailbox-name data-buffer &optional flags date)
  (let ((size  (buffer-size (get-buffer data-buffer)))
        (data  (with-current-buffer data-buffer
                 (buffer-string)))
        message)
    (push (concat "{" (int-to-string size) "}") message)
    (when date
      (push date message))
    (when flags
      (push (format "%s" flags) message))
    (push mailbox-name message)
    (imap-connection-send conn
                          :message (string-join (cons "APPEND" message) " ")
                          :get-response (lambda (resp)
                                          (setf resp-data resp))
                          :provide-reply (lambda (_s)
                                           data))))

;; Selectate state commands

(defun imap-command-check (conn)
  "The CHECK command requests a checkpoint of the currently selected
mailbox.  A checkpoint refers to any implementation-dependent
housekeeping associated with the mailbox (e.g., resolving the
server's in-memory state of the mailbox with the state on its
disk.

Returns the response-code."
  (imap-connection-send conn :message "CHECK"))

(defun imap-command-close (conn)
  "The CLOSE command permanently removes all messages that have the
\Deleted flag set from the currently selected mailbox, and returns
to the authenticated state from the selected state.  No untagged
EXPUNGE responses are sent.

No messages are removed, and no error is given, if the mailbox is
selected by an EXAMINE command or is otherwise selected read-only.

Returns the response code."
  (imap-connection-send conn :message "CLOSE"))

(defun imap-command-expunge (conn)
  "The EXPUNGE command permanently removes all messages that have the
\Deleted flag set from the currently selected mailbox.

Returns cons pair or (response-code . deleted messages list)."
  (let (resp-data)
    (cons (imap-connection-send conn
                                :message "CLOSE"
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

;; TODO (defun imap-command-search (conn query)

(defconst imap--fetch-macro
  '(ALL FAST FULL)
  "ALL
      Macro equivalent to: (FLAGS INTERNALDATE RFC822.SIZE ENVELOPE)

   FAST
      Macro equivalent to: (FLAGS INTERNALDATE RFC822.SIZE)

   FULL
      Macro equivalent to: (FLAGS INTERNALDATE RFC822.SIZE ENVELOPE
BODY)")

(defun imap-command-fetch (conn id-or-range macro-or-data-items)
  (when (not (memq macro-or-data-items imap--fetch-macro))
    (error "FETCH is expected to be a `imap--fetch-macro'"))
  (let (resp-data message)
    (push (symbol-name macro-or-data-items) message)
    (if (consp id-or-range)
        (push (format "%d:%d" (car id-or-range) (cdr id-or-range))
              message)
      (push (int-to-string id-or-range) message))
    (push "FETCH" message)
    (cons (imap-connection-send conn
                                :message (string-join message " ")
                                :get-response (lambda (resp)
                                                (setf resp-data resp)))
          resp-data)))

(defun test-parse ()
  (interactive)
  (print (imap--parse-response-line)))

;; Utils

(define-thing-chars imap--argument "[:alpha:]\|\\|\$")

(defun imap--symbol-name-p (string)
  (let ((case-fold-search nil))
    (and (string-match-p "\\`[A-Z]+\\'" string)
         t)))

(defun imap--parse-response-buffer ()
  "TODO"
  (beginning-of-buffer)
  (let (lines)
    (while (not (eobp))
      (when (eq (following-char) ?* )
        (when-let ((line (imap--parse-response-line)))
          (push line lines)))
      (forward-line))
    (reverse lines)))

(cl-defun imap--parse-response-line ()
  "TODO"
  (move-beginning-of-line 1)
  (print (buffer-substring (line-beginning-position)
                           (line-end-position)))
  (let (tokens
        early-exit)
    (while (not (or (eolp)
                    early-exit))
      (cond
       ;; Skip `*' or tab or space
       ((or (eq (following-char) ?* )
            (eq (following-char) ?\t )
            (eq (following-char) ?\  ))
        (forward-whitespace 1))
       ;; Line ends with \r\n, eolp returns nil on \r
       ((eq (following-char) ?\r )
        (forward-char))
       ;; We found ')' which means we are inspecting internal list
       ;; and must exit immediately
       ((eq (following-char) ?\) )
        (cl-return-from imap--parse-response-line (reverse tokens)))
       ;; Parse list, as in (elem1 elem2), since ) may occure withing
       ;; quoted strings for example, parse recursively substring from
       ;; '(' till the end of the line expecting parser to exit on ')',
       ;; giving us the offset of ')' as a (pointer) position within
       ;; a substring buffer
       ((eq (following-char) ?\( )
        (if-let ((start (1+ (point))))
            (let (offset)
              (with-substring start (line-end-position)
                              (push (imap--parse-response-line) tokens)
                              (setf offset (point)))
              (forward-char (1+ offset)))
          (error "Malformed list")))
       ;; Parse quoted string
       ((eq (following-char) ?\" )
        (if-let ((start (1+ (point)))
                 (end   (progn
                          (forward-char)
                          (search-forward "\"" (line-end-position) 'move))))
            (let ((tok (buffer-substring-no-properties start (1- end))))
              (push tok tokens))
          (error "Malformed quoted string")))
       ;; Else: primitive token
       (t (progn
            (if-let ((num (number-at-point)))
                (progn
                  (push num tokens)
                  (right-word))
              (let ((word (thing-at-point 'imap--argument t)))
                (if (string= word "CAPABILITY")  ;; Special case
                    (progn                       ;; parse capabilities
                      (push 'CAPABILITY tokens)  ;; as raw strings
                      (right-word)
                      (let ((sub (buffer-substring-no-properties
                                  (point)
                                  (line-end-position))))
                        (mapc (lambda (tok) (push (upcase tok) tokens))
                              (split-string sub)))
                      (end-of-line))
                  (if (string= word "OK") ;; Special case, strings
                      (progn              ;; like OK [ ..data.. ] Comments
                        (push 'OK tokens)
                        (push (imap--parse-response-ok) tokens))
                    (push (if (imap--symbol-name-p word) (intern word) word) tokens)
                    (end-of-thing 'imap--argument)))))))))
    (reverse tokens)))

(defun imap--parse-response-ok ()
  (let (result)
    (when-let ((start (search-forward "[" (line-end-position) 'move))
               (end   (search-forward "]" (line-end-position) 'move)))
      (with-substring start (1- end)
        (message (format "buffer string: %s" (buffer-string)))
        (setq result (imap--parse-response-line))))
    (end-of-line)
    result))

(defmacro with-substring (start end &rest body)
  `(let ((substring (buffer-substring-no-properties ,start ,end)))
     (with-temp-buffer
       (insert substring)
       ,@body)))

(cl-defun imap-connection-send (conn &key message get-response (provide-reply (lambda (_s) "")))
  "TODO"
  (let ((command-id (imap-connection-command-id conn))
        (proc       (imap-connection-proc conn))
        openp)
    ;; TODO quit SIGNAL
    (condition-case nil
        (with-current-buffer (imap-connection-buffer conn)
          (erase-buffer)
          (process-send-string proc
                               (format "%d %s\r\n"
                                       command-id
                                       message))
          (goto-char (point-max))
          (while (and (setq openp (memq (process-status proc)
				        '(open run)))
	              (progn
		        ;; Skip past any "*" lines that the server has
		        ;; output.
		        (while (and (not (bobp))
			            (progn
			              (forward-line -1)
                                      (or (looking-at "\\*\\|[0-9]+ OK NOOP")
                                          (and (looking-at "\\+")
                                               (let ((s (buffer-substring-no-properties
                                                         (line-beginning-position)
                                                         (line-end-position))))
                                                 (process-send-string proc
                                                                      (format "%s\r\n" (funcall provide-reply s)))
                                                 (kill-whole-line)
                                                 t))))))
		        (not (looking-at (format "%d .*\n" command-id)))))
            (accept-process-output proc 2)
            (goto-char (point-max)))
          (let ((res (cond ((looking-at "[0-9]+ OK") 'ok)
                           ((looking-at "[0-9]+ BAD") 'bad)
                           ((looking-at "[0-9]+ NO") 'no)
                           (t (error "Unknown response")))))
            (when (and (eq res 'ok) (functionp get-response))
              (narrow-to-region (point-min) (point))
              (funcall get-response (imap--parse-response-buffer))
              (widen))
            (cl-incf (imap-connection-command-id conn))
            res))
      (quit
       ;; TODO log error
       (delete-process proc)))))

(defun imap--base64-encode (string &optional encoding)
  (with-temp-buffer
    (let (;;(string (string-as-unibyte (encode-coding-string string encoding)))
          (encoding (or encoding 'utf-16))
          (state 'ascii)
          start)
      (dotimes (pos (length string))
        (let ((char (aref string pos)))
          (if (and (>= char #x20)  ;; Char is ascii
                   (<= char #x7e))
              (progn
                (when (eq state 'utf)
                  (setf state 'ascii)
                  (message (format "Encoding: %s" (buffer-substring start (point)))) 
                  (encode-coding-region start (point) encoding)
                  (base64-encode-region start (point))
                  (delete-char -1)
                  (insert ?-))
                (if (eq char ?&)
                    (insert "&-")
                  (insert char)))
            (when (eq state 'ascii) ;; Multibyte
              (insert ?&)
              (setf start (point)
                    state 'utf))
            (insert char))))
      (buffer-string))))

(defun imap--base64-decode (string &optional encoding)
  (with-temp-buffer
    (let ((encoding (or encoding 'utf-16))
          (state 'ascii)
          start)
      (dotimes (pos (length string))
        (let ((char (aref string pos)))
          (cl-case char
            (?& (setf state 'maybe-base64
                      start (point)))
            (?- (cl-case state
                  ('maybe-base64 (insert ?&))
                  ('ascii        (insert char))
                  ('base64  (progn
                              (setf state 'ascii)
                              (insert ?=)
                              (base64-decode-region start (point))
                              (decode-coding-region start (point) encoding)))))
            (t  (progn
                  (when (eq state 'maybe-base64)
                    (setf state 'base64))
                  (insert char))))))
      (message (buffer-string))
      (buffer-string))))

(provide 'rfc3501)
