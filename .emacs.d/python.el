;;; python.el --- Python configuration with LSP support -*- lexical-binding: t -*-

;; Python mode (built-in) with LSP
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'python-setup-buffer)

;; Pyright LSP (faster, better type checking)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; Virtual environment support
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)
  ;; Auto-activate venv if .venv or venv directory exists
  (add-hook 'python-mode-hook
            (lambda ()
              (when-let* ((root (find-project-file "pyproject.toml"))
                          (venv (or (concat root ".venv")
                                    (concat root "venv"))))
                (when (file-directory-p venv)
                  (pyvenv-activate venv))))))

;; Pytest integration
(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
              ("C-c t t" . python-pytest-dispatch)
              ("C-c t f" . python-pytest-file)
              ("C-c t m" . python-pytest-function)))

;; Buffer setup
(defun python-setup-buffer ()
  "Setup Python buffer with formatting and development features."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local python-indent-offset 4)

  ;; Format on save with black or ruff
  (add-hook 'before-save-hook #'python-format-buffer nil t)

  ;; Set compile command
  (when (buffer-file-name)
    (set (make-local-variable 'compile-command)
         (format "python %s" (shell-quote-argument (buffer-file-name))))))

;; Formatting with black or ruff
(defun python-format-buffer ()
  "Format Python buffer with ruff or black."
  (interactive)
  (when (eq major-mode 'python-mode)
    (cond
     ((executable-find "ruff")
      (let ((file (buffer-file-name)))
        (when file
          (call-process "ruff" nil nil nil "format" file)
          (revert-buffer t t t))))
     ((executable-find "black")
      (let ((file (buffer-file-name)))
        (when file
          (call-process "black" nil nil nil "-q" file)
          (revert-buffer t t t)))))))

;; Utility functions
(defun python-run-file ()
  "Run current Python file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (compile (format "python %s" (shell-quote-argument file))))))

(defun python-run-tests ()
  "Run pytest on current project."
  (interactive)
  (let ((root (find-project-file "pyproject.toml")))
    (if root
        (let ((default-directory root))
          (compile "pytest"))
      (message "No pyproject.toml found"))))

(defun python-pip-install ()
  "Run pip install -e . in project root."
  (interactive)
  (let ((root (find-project-file "pyproject.toml")))
    (if root
        (let ((default-directory root))
          (compile "pip install -e ."))
      (message "No pyproject.toml found"))))

;; Key bindings
(defun python-setup-keybindings ()
  "Setup Python-specific key bindings."
  (local-set-key (kbd "C-c r r") 'python-run-file)
  (local-set-key (kbd "C-c r t") 'python-run-tests)
  (local-set-key (kbd "C-c f f") 'python-format-buffer)
  (local-set-key (kbd "C-c p i") 'python-pip-install))

(add-hook 'python-mode-hook 'python-setup-keybindings)

(provide 'python-config)
;;; python.el ends here
