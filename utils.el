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

(provide 'utils)
;;; utils.el ends here