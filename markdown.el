;;; markdown.el --- Markdown mode configuration

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :hook
  ((markdown-mode . visual-line-mode)
   (markdown-mode . (lambda ()
                     (message "Setting up prettier hooks for markdown-mode")
                     (add-hook 'after-save-hook #'markdown-prettier-after-save nil t))))
  :config
  (defun markdown-format-with-prettier ()
    "Format the current Markdown buffer using Prettier via shell-command."
    (interactive)
    (message "markdown-format-with-prettier called for %s" (buffer-file-name))
    (when (and (eq major-mode 'markdown-mode)
               (buffer-file-name)
               (file-exists-p "/opt/homebrew/bin/prettier"))
      (let ((file (shell-quote-argument (buffer-file-name)))
            (output-buffer "*Prettier Output*"))
        (with-current-buffer (get-buffer-create output-buffer)
          (erase-buffer))
        (let ((exit-code (call-process "/opt/homebrew/bin/prettier" nil output-buffer nil "--write" file)))
          (if (zerop exit-code)
              (progn
                (revert-buffer t t t)
                (message "Formatted %s with Prettier" file))
            (message "Prettier failed with exit code %d. See %s for details" exit-code output-buffer))))
      (unless (and (buffer-file-name) (file-exists-p "/opt/homebrew/bin/prettier"))
        (message "Cannot format: Buffer not visiting a file or Prettier not found"))))
  
  (defun markdown-prettier-after-save ()
    "Format markdown with prettier after saving."
    (when (and (eq major-mode 'markdown-mode)
               (buffer-file-name))
      (message "Running prettier on %s after save..." (buffer-file-name))
      (markdown-format-with-prettier)))
  
  :bind
  (:map markdown-mode-map
        ("C-c C-f" . markdown-format-with-prettier)))

(provide 'markdown)
;;; markdown.el ends here