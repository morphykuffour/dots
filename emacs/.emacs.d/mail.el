;; mail setup
;; https://github.com/DiamondBond/emacs/blob/master/config.org#prerequisites
(defun mu-setup/init-mu ()
  "Initializes 'mu' db."
  (interactive)
  (async-shell-command "/usr/local/bin/mu init --maildir=/home/morp/mail/ --my-address=your-email@example.com"))

(defun mu-setup/build-mu-index ()
  "Builds 'mu' index."
  (interactive)
  (async-shell-command "/usr/local/bin/mu  index"))

(add-to-list  'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(require 'org-mu4e)
(setq mu4e-maildir (expand-file-name "~/mail"))

;; set folders
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; composing mail
(setq mu4e-compose-dont-reply-to-self t)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; display options
(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses 't)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

    ;; mu4e toggle html images
    (defvar killdash9/mu4e~view-html-images nil
      "Whether to show images in html messages")
   
    (defun killdash9/mu4e-view-toggle-html-images ()
      "Toggle image-display of html message."
      (interactive)
      (setq-local killdash9/mu4e~view-html-images (not killdash9/mu4e~view-html-images))
      (message "Images are %s" (if killdash9/mu4e~view-html-images "on" "off"))
      (mu4e-view-refresh))

    (defun mu4e-shr2text (msg)
      "Convert html in MSG to text using the shr engine; this can be
used in `mu4e-html2text-command' in a new enough emacs. Based on
code by Titus von der Malsburg."
      (lexical-let ((view-images killdash9/mu4e~view-html-images))
        (mu4e~html2text-wrapper
         (lambda ()
	       (let ((shr-inhibit-images (not view-images)))
	         (shr-render-region (point-min) (point-max)))) msg)))
   
    (define-key mu4e-view-mode-map "i" 'killdash9/mu4e-view-toggle-html-images)



;; make sure that moving a message (like to Trash) causes the
;; message to get a new file name.  This helps to avoid the
;; dreaded "UID is N beyond highest assigned" error.
;; See this link for more info: https://stackoverflow.com/a/43461973
(setq mu4e-change-filenames-when-moving t)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '( (:maildir "/INBOX"             :key ?i)
        (:maildir "/[Gmail].Sent Mail"  :key ?s)
        (:maildir "/[Gmail].Trash"      :key ?t)
        (:maildir "/[Gmail].All Mail"   :key ?a)))

;; inbox-query
(setq db/mu4e-inbox-query
      "(maildir:/Inbox OR maildir:/INBOX) AND flag:unread")

;; go-to-inbox function
(defun db/go-to-inbox ()
  (interactive)
  (mu4e-headers-search dw/mu4e-inbox-query))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; why would I want to leave my message open after I've sent it?
(setq message-kill-buffer-on-exit t)
;; don't ask for a 'context' upon opening mu4e
(setq mu4e-context-policy 'pick-first)
;; don't ask to quit
(setq mu4e-confirm-quit nil)

;; function to sync mail
(defun sync/mail ()
  "Sync email."
  (interactive)
  (async-shell-command "offlineimap")
  (mu4e-update-index))

(use-package smtpmail
             ;; :straight t
             :config
             (setq message-send-mail-function 'smtpmail-send-it
                   starttls-use-gnutls t
                   user-mail-address "your-email@example.com"
                   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
                   smtpmail-auth-credentials (expand-file-name "~/.offlineimappass.gpg")
                   smtpmail-smtp-user "morpkuff"
                   smtpmail-local-domain "gmail.com"
                   smtpmail-default-smtp-server "smtp.gmail.com"
                   smtpmail-smtp-server "smtp.gmail.com"
                   smtpmail-smtp-service 587
                   smtpmail-debug-info t))

