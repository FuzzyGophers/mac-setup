;;; markdown.el --- Markdown mode configuration -*- lexical-binding: t -*-

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :hook
  ((markdown-mode . visual-line-mode)
   (markdown-mode . (lambda ()
                      (add-hook 'after-save-hook #'markdown-prettier-after-save nil t))))
  :config
  (defun markdown-format-with-prettier ()
    "Format the current Markdown buffer using Prettier via shell-command."
    (interactive)
    (when-let* ((prettier (executable-find "prettier"))
                (file (buffer-file-name)))
      (when (eq major-mode 'markdown-mode)
        (let ((output-buffer "*Prettier Output*"))
          (with-current-buffer (get-buffer-create output-buffer)
            (erase-buffer))
          (let ((exit-code (call-process prettier nil output-buffer nil "--write" (shell-quote-argument file))))
            (if (zerop exit-code)
                (revert-buffer t t t)
              (message "Prettier failed with exit code %d" exit-code)))))))
  
  (defun markdown-prettier-after-save ()
    "Format markdown with prettier after saving."
    (when (and (eq major-mode 'markdown-mode)
               (buffer-file-name))
      (markdown-format-with-prettier)))
  
  :bind
  (:map markdown-mode-map
        ("C-c C-f" . markdown-format-with-prettier)))

(provide 'markdown)
;;; markdown.el ends here