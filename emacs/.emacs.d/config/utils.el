;;; utils.el --- Utility Functions -*- lexical-binding: t -*-

(defun insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date"
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p)))
(defun copy-filename()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                    default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


(defun reload-config ()
  "Reload Emacs Configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;; WSL specific
(defun copy-selected-text (start end)
  (interactive "r")
  (if (use-region-p)
    (let ((text (buffer-substring-no-properties start end)))
      (shell-command (concat "echo '" text "' | clip.exe")))))

;; package install highlight
(defun highlight-line-dups ()
  (interactive)
  (let ((count  0)
        line-re)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq count    0
              line-re  (concat "^" (regexp-quote (buffer-substring-no-properties
                                                  (line-beginning-position)
                                                  (line-end-position)))
                               "$"))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (if (not (re-search-forward line-re nil t))
                (goto-char (point-max))
              (setq count  (1+ count))
              (unless (< count 2)
                (hlt-highlight-region (line-beginning-position) (line-end-position)
                                      'font-lock-warning-face)
                (forward-line 1)))))
        (forward-line 1)))))

;; Take a screenshot: https://stackoverflow.com/a/31868530
(defun org-screenshot ()
"Take a screenshot into a time stamped unique-named file in ~/Org/imgs
and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (let* ((imgs-dir (expand-file-name "~/Org/imgs/"))
         (timestamp (format-time-string "%Y%m%d_%H%M%S_"))
         (filename (concat imgs-dir (make-temp-name timestamp) ".png")))
    (unless (file-exists-p imgs-dir)
      (make-directory imgs-dir t))
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))

  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]")))))
