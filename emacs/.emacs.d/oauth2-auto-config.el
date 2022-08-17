;;; oauth2-token-fetch.el -*- lexical-binding: t; -*-

(let ((username (nth 3 command-line-args))
      (provider (nth 4 command-line-args))
      (repo-dir
       (concat straight-base-dir ".local/straight/repos/")))

  ; very basic parsing of command line arguments
  (unless (and username provider)
    (princ "Could not find username or provider.\n")
    (princ "emacs --script fetch-oauth-token.el USERNAME PROVIDER\n")
    (kill-emacs 1))

  ; Load and configure the GPG encryption by `plstore'
  (require 'plstore)
  (setq plstore-encrypt-to "my-email@example.com")

  ; Load dependencies
  (require 'aio (concat repo-dir "emacs-aio/aio.el"))
  (require 'dash (concat repo-dir "dash.el/dash.el"))
  (require 'alert (concat repo-dir "alert/alert.el"))
  (require 'oauth2-auto (concat repo-dir "emacs-oauth2-auto-oauth2-auto.el"))

  ; Load client ID, secrets and tenants
  (load-file "/path/to/oauth2-auto-config.el")

  ; Authenticate user and print access token
  (princ (oauth2-auto-access-token-sync username (intern provider)))
  (princ "\n"))
