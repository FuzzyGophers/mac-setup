;;; utils.el --- Common utilities for Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Common utility functions to reduce redundancy across configuration files

;;; Code:

(require 'package)

(defun ensure-package-installed (package)
  "Ensure that PACKAGE is installed, installing it if necessary."
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))

(defun setup-format-on-save (format-function &optional file-pattern)
  "Setup FORMAT-FUNCTION to run on save for current buffer.
If FILE-PATTERN is provided, only format files matching the pattern."
  (add-hook 'before-save-hook
            (lambda ()
              (when (or (null file-pattern)
                        (string-match file-pattern (or (buffer-file-name) "")))
                (funcall format-function)))
            nil t))

(defun setup-lsp-mode ()
  "Common LSP mode setup."
  (lsp)
  (company-mode t)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        lsp-headerline-breadcrumb-enable t))

(defun add-executable-hook ()
  "Make file executable if it has a shebang."
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;; Format buffer with external command
(defun format-buffer-with-command (command &rest args)
  "Format current buffer with COMMAND and ARGS.
Returns t on success, nil on failure."
  (when-let* ((executable (executable-find command))
              (file (buffer-file-name)))
    (let ((output-buffer "*Format Output*"))
      (with-current-buffer (get-buffer-create output-buffer)
        (erase-buffer))
      (let ((exit-code (apply #'call-process executable nil output-buffer nil
                              (append args (list (shell-quote-argument file))))))
        (if (zerop exit-code)
            (progn (revert-buffer t t t) t)
          (message "%s failed with exit code %d" command exit-code)
          nil)))))

;; Project detection helpers
(defun find-project-file (filename)
  "Find FILENAME in current project root by searching up the directory tree."
  (when (buffer-file-name)
    (locate-dominating-file (buffer-file-name) filename)))

(defun find-project-root ()
  "Find project root by looking for common project markers."
  (or (find-project-file ".git")
      (find-project-file "package.json")
      (find-project-file "Cargo.toml")
      (find-project-file "go.mod")
      (find-project-file "pyproject.toml")
      (find-project-file "setup.py")
      default-directory))

(provide 'utils)
;;; utils.el ends here