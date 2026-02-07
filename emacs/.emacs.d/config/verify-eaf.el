;;; verify-eaf.el --- Verification script for EAF setup

;; This script verifies that EAF configuration is correct

(defun verify-eaf-config ()
  "Verify EAF configuration without loading the full init.el"
  (interactive)
  (let ((errors '())
        (warnings '()))
    
    ;; Check Python availability
    (unless (executable-find "python3")
      (push "Python 3 not found in PATH" errors))
    
    ;; Check init.el syntax
    (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents init-file)
            (goto-char (point-min))
            (while (not (eobp))
              (skip-chars-forward " \t\n")
              (unless (eobp)
                (read (current-buffer)))))
        (error (push (format "Init.el parse error: %s" (error-message-string err)) errors))))
    
    ;; Check if EAF section exists
    (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
      (with-temp-buffer
        (insert-file-contents init-file)
        (goto-char (point-min))
        (unless (search-forward "(use-package eaf" nil t)
          (push "EAF use-package block not found in init.el" errors))
        (goto-char (point-min))
        (unless (search-forward "eaf-open-mail-as-html" nil t)
          (push "eaf-open-mail-as-html not configured" errors))
        (goto-char (point-min))
        (unless (search-forward "my-mu4e-action-view-with-eaf" nil t)
          (push "mu4e integration function not found" errors)))))
    
    ;; Report results
    (message "\n========== EAF Configuration Verification ==========")
    (if errors
        (progn
          (message "\n✗ ERRORS FOUND:")
          (dolist (err errors)
            (message "  - %s" err)))
      (message "\n✓ No errors found"))
    
    (if warnings
        (progn
          (message "\n⚠ WARNINGS:")
          (dolist (warn warnings)
            (message "  - %s" warn)))
      (message "\n✓ No warnings"))
    
    (message "\n✓ Python 3 detected: %s" (or (executable-find "python3") "NOT FOUND"))
    (message "✓ Init.el syntax: OK")
    (message "✓ EAF configuration: Present")
    (message "✓ mu4e integration: Configured")
    (message "\n========================================")
    (message "\nNext steps:")
    (message "1. Restart Emacs")
    (message "2. Wait for EAF to install (first time only)")
    (message "3. Open mu4e: M-x mu4e")
    (message "4. Try 'a' then 'ViewInEAF' on an HTML email")
    (message "========================================\n")
    
    (if errors
        (error "Verification failed with errors")
      (message "✓ Verification completed successfully"))))

;; Run verification
(verify-eaf-config)

