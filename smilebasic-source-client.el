;;; smilebasic-source-client.el --- Client for SmileBASIC Source
;;; Commentary:
;; This client is made specifically for chat.  I will add more features if I feel like
;; adding more.
;; NEED TO IMPLEMENT MESSAGE SEPERATING IF LONGER THAN 3 MINUTES
;;; Code:

(require 'json)
(require 'cl-lib)

(setq debug-on-error t)

(defgroup smilebasic-source-client nil
  "SmileBASIC Source EMACS Client"
  :prefix "smilebasic-source-" :group 'comm
  :version "1.0")

(defcustom smilebasic-source-base-url "newdev.smilebasicsource.com"
  "The URL where all API activity is retrieved from."
  :type 'string
  :group 'smilebasic-source-client)

(defcustom smilebasic-source-authtoken nil
  "The authentification key."
  :type 'string
  :group 'smilebasic-source-client)

;;; Global variables:
(defvar smilebasic-source-minibuffer-chat nil
  "The chat buffer that will show active messages in the minibuffer.  If it is non-nil, then it will display new messages from that buffer in real-time.  Only one buffer can be selected at a time.  This will also be the buffer that is selected when doing quick replies.")

(defvar smilebasic-source-chat-buffers nil
  "The list of chat buffers that the timer will go through and update in real time.")

(defvar smilebasic-source-chat-buffers-ids nil
  "The list of chat buffers' IDs.")

(defvar smilebasic-source-chat-updating nil
  "Are the chat buffers currently being updated?")

(defvar smilebasic-source-chat-update-buffer nil
  "The buffer currently being processed in the timer processed.  This exists because we can't pass the buffer asynchronously.")

(defvar smilebasic-source-listener-last-id 0
  "The last sent ID that the listener process has read.")

;;; Local variables for Chat buffers:
(defvar smilebasic-source-chat-buffer-page-id nil
  "The page ID associated to the buffer.")

(defvar smilebasic-source-chat-buffer-last-sent-id nil
  "The ID of the last sent message to the chat buffer.")

(defvar smilebasic-source-chat-buffer-last-sent-uid nil
  "The ID of the last user who sent a message to the chat buffer.")

;;; Helper functions:
;; Buffer manipulation
(defun smilebasic-source-request-buffer-trim ()
  "When making a request, it often comes with headers that is worth nothing."
  (goto-char (point-min))
  (re-search-forward "^$")
  (delete-region (point) (point-min)))

(defun smilebasic-source-request-buffer-json ()
  "Read the buffer as JSON."
  (smilebasic-source-request-buffer-trim)
  (if (string-match-p (regexp-quote "{") (buffer-string))
      (if (version< emacs-version "27")
	  (let ((json-false nil))
	    (json-read))
	(condition-case nil
	    (json-parse-buffer :object-type 'alist)
	  (json-parse-error nil)))))

;; Avatars
(defconst smilebasic-source-avatar-format "%ssbs2_av_%d_%d.png"
  "The formatting used to save avatars in the temp storage.")

(defconst smilebasic-source-avatar-url "https://%s/api/File/raw/%d?size=%d&crop=true"
  "The URl used to get an avatar file.")

(defcustom smilebasic-source-avatar-size 20
  "The size of the avatars used for the client."
  :type 'number
  :group 'smilebasic-source-client)

(defun smilebasic-source-avatar-get-sized (file-id avatar-size)
  "Return an avatar image with ID FILE-ID of size AVATAR-SIZE."
  (let (
	(filename (format
		   smilebasic-source-avatar-format
		   temporary-file-directory
		   avatar-size
		   file-id)))
    (unless (file-exists-p filename)
      (url-copy-file (format
		      smilebasic-source-avatar-url
		      smilebasic-source-base-url
		      file-id
		      avatar-size)
		     filename))
    (create-image filename)))

(defun smilebasic-source-avatar-get (file-id)
  "Return an avatar image with ID FILE-ID with configured size."
  (smilebasic-source-avatar-get-sized file-id smilebasic-source-avatar-size))

;;; Icons:
(defconst smilebasic-source-icon-format "%ssbs2_ico_%s_%d.png"
  "The name format given to icons being saved in the filesystem.")

(defconst smilebasic-source-content-icon-url "https://newdev.smilebasicsource.com/12/icons/page.png"
  "The URL where to get the icon used to represent content.")

(defconst smilebasic-source-content-icon-name "content"
  "The name of the file of the icon used to represent content.")

(defun smilebasic-source-icon-get (url name size)
  "Wrapper to get icons from a specified URL saved as NAME in the filesystem of size SIZE."
  (let (
	(filename (format
		   smilebasic-source-icon-format
		   temporary-file-directory
		   name
		   size)))
    (unless (file-exists-p filename)
      (url-copy-file (concat url (format "?size=%d&crop=true" size)) filename)
      (create-image filename))))

(defun smilebasic-source-icon-content-get (size)
  "Return the icon used for content of a specified SIZE."
  (smilebasic-source-icon-get
   smilebasic-source-content-icon-url
   smilebasic-source-content-icon-name
   size))

;;; Login:
(defconst smilebasic-source-login-url "https://%s/api/User/authenticate"
  "The URL used to get an AuthToken.")

(defun smilebasic-source-login-gettoken (username password)
  "Return an AuthToken from the server using a USERNAME and PASSWORD."
  (let (
	(url (format
	      smilebasic-source-login-url
	      smilebasic-source-base-url))
	(url-request-method "POST")
	(url-request-extra-headers (list
				    (cons "Content-Type" "text/json")))
	(url-request-data (json-encode
			   (list
			    (cons "username" username)
			    (cons "password" password)))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (smilebasic-source-request-buffer-trim))))

(defun smilebasic-source-login ()
  "Login to SmileBASIC Source and set AuthToken."
  (interactive)
  (let (
	(username (read-string "Username: "))
	(password (read-string "Password: ")))
    (setq smilebasic-source-authtoken (smilebasic-source-login-gettoken username password))))

;;; Chat:
(define-derived-mode smilebasic-source-chat-mode fundamental-mode "SmileBASIC Source Chat"
  "The mode used to view SmileBASIC Source pages in chat view."
  (goto-address-mode t)
  (visual-line-mode t)
  (local-set-key "w" #'smilebasic-source-chat-send-message))

(defconst smilebasic-source-chat-initial-request-url "https://%s/api/Read/chain/?requests=content-%s&content=name,id&requests=comment-%s&requests=user.1createUserId&content.1parentId"
  "The request that is made to initialize chat buffers.")

(defconst smilebasic-source-chat-listen-url "https://%s/api/Read/listen?actions=%s"
  "The request that is made to listen to new messages.")

(defun smilebasic-source-chat-listen-gen-url ()
  "Generate a URL that is used to listen to new messages when called in relation to the last sent ID."
  (format
   smilebasic-source-chat-listen-url
   smilebasic-source-base-url
   (json-encode (list
		 (cons "lastId" smilebasic-source-listener-last-id)
		 '("chains" . ["comment.0id" "user.1createUserId" "content.1parentId"])))))

;; TODO: REFACTOR THE IMPLEMENTATION, THIS IS COPIED DIRECTLY FROM OLD CLIENT.
;; COULD BE TERRIBLY INEFFICIENT
(defun smilebasic-source-chat-insert-comments (comments users contents)
  "Insert COMMENTS into the buffer.  Refers to the list of USERS in order to grab information related to the users who sent the comments.  I love CONTENTS."
  (unless (= (length comments) 0)
    (let ((value '()) (max-i (length comments)) (i nil) (n nil))
      (dotimes (n max-i value)
	(setq i (- max-i (+ 1 n)))
	(unless (= (cdr (assoc 'createUserId (aref comments i))) 0)
	  ;; get current user of the message
	  (let ((username) (user
			    (let ((uid (cdr (assoc 'createUserId (aref comments i)))) (value nil) (max-j (length users)) (j nil))
			      (dotimes (j max-j value)
				(when (= uid (cdr (assoc 'id (aref users j))))
				  (setq value (aref users j)))))))
	    (setq username (cdr (assoc 'username user)))
	    (put-text-property 0 (length username) 'face 'bold username)
	    (unless user
	      (error "USER LIST AND COMMENT LIST DO NOT MATCH"))
	    (unless (= smilebasic-source-chat-buffer-last-sent-uid (cdr (assoc 'id user)))
	      (if window-system
		  (progn
		    (insert-image (smilebasic-source-avatar-get (cdr (assoc 'avatar user))))
		    (insert " "))
		(insert "\n"))
	      (let ((sendtime (concat "[" (substring (cdr (assoc 'createDate (aref comments i))) 11 16) "]")))
		(put-text-property 0 (length sendtime) 'face 'shadow sendtime)
		(insert username ": " sendtime "\n")))
	    (let ((cnt (cdr (assoc 'content (aref comments i)))))
	      (insert (substring cnt (+ (string-match "\n" cnt) 1)) "\n"))
	    (setq-local smilebasic-source-chat-buffer-last-sent-uid (cdr (assoc 'id user)))
	    (setq-local smilebasic-source-chat-buffer-last-sent-id (cdr (assoc 'id (aref comments i)))))))))
  smilebasic-source-chat-buffer-last-sent-id)

(defun smilebasic-source-chat-looper (x)
  "This is the loop that will update the buffers.  X should be ignored."
  (let ((json-data (smilebasic-source-request-buffer-json)))
    (when json-data
      (let ((comments
	     (cdr (assoc 'comment (cdr (assoc 'chains json-data)))))
	    (users
	     (cdr (assoc 'user (cdr (assoc 'chains json-data)))))
	    (contents
	     (cdr (assoc 'content (cdr (assoc 'chains json-data))))))
	(let ((last-id smilebasic-source-listener-last-id) (i 0))
	  (while (> (length comments) i)
	    (let ((current-comment (aref comments i)))
	      (setq smilebasic-source-listener-last-id (cdr (assoc 'id current-comment)))
	      (let ((this-parent-id (cdr (assoc 'parentId current-comment))))
		(when (member this-parent-id smilebasic-source-chat-buffers-ids)
		  (with-current-buffer (nth (cl-position this-parent-id smilebasic-source-chat-buffers-ids) smilebasic-source-chat-buffers)
		    (goto-char (point-max))
		    (smilebasic-source-chat-insert-comments (vector current-comment) users contents)))))
	    (setq i (+ i 1)))))))
  (smilebasic-source-chat-listen))

(defun smilebasic-source-chat-send-message (&optional contents)
  "Sends a message in the current buffer the CONTENTS."
  (interactive)
  (let ((pid smilebasic-source-chat-buffer-page-id) (bufname (buffer-name)))
    (unless contents (setq contents (read-string (concat "#" bufname ": "))))
    (unless (string= contents "")
      (setq contents (concat (json-encode (list (cons "m" "12y"))) "\n" contents))
      (let (
	    (url-request-method "POST")
	    (url-request-extra-headers
	     (list
	      (cons "Authorization"
		    (format "Bearer %s" smilebasic-source-authtoken))
	      '("Content-Type" . "text/json")))
	    (url-request-data
	     (json-encode
	      (list (cons "parentId" pid) (cons "content" contents)))))
	(with-temp-buffer
	  (url-retrieve (format "http://%s/api/Comment" smilebasic-source-base-url)
			(lambda (x)()) nil t))))))

(defun smilebasic-source-chat-listen ()
  "Call listen protocol and update accordingly."
  (setq smilebasic-source-chat-updating t)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 (list (cons "Authorization"
		     (format "Bearer %s" smilebasic-source-authtoken)))))
    (url-retrieve
     (smilebasic-source-chat-listen-gen-url)
     #'smilebasic-source-chat-looper)))

(defun smilebasic-source-chat-open-page (id)
  "Opens a chat buffer of page ID."
  (if (member id smilebasic-source-chat-buffers-ids)
      (switch-to-buffer
       (nth (cl-position id smilebasic-source-chat-buffers-ids) smilebasic-source-chat-buffers))
    (let
	;; OPTIMIZE: Need to have it so it encodes with either the JSON
	;; library or the built-in Jansson library depending on the version
	;; of Emacs.
	((url (format
	       smilebasic-source-chat-initial-request-url
	       smilebasic-source-base-url
	       (json-encode (list
			     (cons "ids" (vector id))))
	       (json-encode (list
			     (cons "parentIds" (vector id))
			     '("reverse" . t)
			     '("limit" . 50)))))
	 (url-request-method "GET"))
      (with-temp-buffer
	(url-retrieve
	 url
	 (lambda (x)
	   (progn
	     (let ((jsondata
		    (smilebasic-source-request-buffer-json)))
	       (let ((buffer
		      (generate-new-buffer
		       (cdr (assoc 'name
				   (aref (cdr (assoc 'content jsondata)) 0))))))
		 (switch-to-buffer buffer)
		 (add-to-list 'smilebasic-source-chat-buffers buffer)
		 (add-to-list 'smilebasic-source-chat-buffers-ids
			      (cdr (assoc 'id
					  (aref (cdr (assoc 'content jsondata)) 0))))

		 (smilebasic-source-chat-mode)

		 (setq-local smilebasic-source-chat-buffer-last-sent-id -1)
		 (setq-local smilebasic-source-chat-buffer-last-sent-uid 0)
		 (setq-local smilebasic-source-chat-buffer-page-id
			     (cdr (assoc 'id
					 (aref (cdr (assoc 'content jsondata)) 0))))

		 (let ((comments (cdr (assoc 'comment jsondata)))
		       (users (cdr (assoc 'user jsondata)))
		       (contents (cdr (assoc 'content jsondata))))
		   (let ((pages-last-id
			  (smilebasic-source-chat-insert-comments comments
								  users
								  contents)))
		     (when (> pages-last-id smilebasic-source-listener-last-id)
		       (setq smilebasic-source-listener-last-id pages-last-id))
		     (unless smilebasic-source-chat-updating
		       (smilebasic-source-chat-listen)))))))))))))

(defun smilebasic-source-client-open-test ()
  "Open a chat buffer at ID 384 as a test."
  (interactive)
  (smilebasic-source-chat-open-page 384))

(provide 'smilebasic-source-client)
;;; smilebasic-source-client.el ends here
