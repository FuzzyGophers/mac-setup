;; Ensure package system is initialized
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents t))

;; Ensure markdown-mode is installed
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;; Configure markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "pandoc")

;; Enable visual line mode and spell-checking
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Format with Prettier via shell-command
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

;; Auto-format on save - using after-save-hook
(defun markdown-prettier-after-save ()
  "Format markdown with prettier after saving."
  (when (and (eq major-mode 'markdown-mode)
             (buffer-file-name))
    (message "Running prettier on %s after save..." (buffer-file-name))
    (markdown-format-with-prettier)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (message "Setting up prettier hooks for markdown-mode")
            (add-hook 'after-save-hook #'markdown-prettier-after-save nil t)))

;; Keybinding for manual formatting
(define-key markdown-mode-map (kbd "C-c C-f") 'markdown-format-with-prettier)
