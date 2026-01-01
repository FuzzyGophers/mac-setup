;;; rust.el --- Rust configuration with LSP support -*- lexical-binding: t -*-

;; Rust mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . rust-setup-buffer))
  :config
  ;; Format on save with rustfmt
  (setq rust-format-on-save t))

;; Cargo integration
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode)
  :bind (:map rust-mode-map
              ("C-c c b" . cargo-process-build)
              ("C-c c r" . cargo-process-run)
              ("C-c c t" . cargo-process-test)
              ("C-c c c" . cargo-process-check)
              ("C-c c l" . cargo-process-clippy)
              ("C-c c d" . cargo-process-doc)))

;; Buffer setup
(defun rust-setup-buffer ()
  "Setup Rust buffer with development features."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)

  ;; Set compile command
  (when (buffer-file-name)
    (set (make-local-variable 'compile-command) "cargo build")))

;; LSP configuration for rust-analyzer
(with-eval-after-load 'lsp-mode
  ;; rust-analyzer settings
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-import-merge-behaviour "full")
  (setq lsp-rust-analyzer-completion-add-call-parenthesis t))

;; Utility functions
(defun rust-cargo-new (name)
  "Create a new Rust project with cargo."
  (interactive "sProject name: ")
  (let ((default-directory (read-directory-name "Create in: ")))
    (compile (format "cargo new %s" name))))

(defun rust-cargo-add (crate)
  "Add a crate dependency."
  (interactive "sCrate name: ")
  (let ((root (find-project-file "Cargo.toml")))
    (if root
        (let ((default-directory root))
          (compile (format "cargo add %s" crate)))
      (message "No Cargo.toml found"))))

(defun rust-cargo-update ()
  "Update dependencies."
  (interactive)
  (let ((root (find-project-file "Cargo.toml")))
    (if root
        (let ((default-directory root))
          (compile "cargo update"))
      (message "No Cargo.toml found"))))

;; Key bindings
(defun rust-setup-keybindings ()
  "Setup Rust-specific key bindings."
  (local-set-key (kbd "C-c c n") 'rust-cargo-new)
  (local-set-key (kbd "C-c c a") 'rust-cargo-add)
  (local-set-key (kbd "C-c c u") 'rust-cargo-update))

(add-hook 'rust-mode-hook 'rust-setup-keybindings)

(provide 'rust-config)
;;; rust.el ends here
