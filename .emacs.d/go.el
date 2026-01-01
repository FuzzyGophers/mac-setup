;;; go.el --- Go language configuration with modern development features -*- lexical-binding: t -*-

;; Add go binaries to PATH (improved path handling)
(let ((go-bin-path (expand-file-name "~/go/bin")))
  (when (file-directory-p go-bin-path)
    (setenv "PATH" (concat (getenv "PATH") ":" go-bin-path))
    (add-to-list 'exec-path go-bin-path)))

;; Core Go mode with LSP
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . go-setup-buffer))
  :config
  (defun go-setup-buffer ()
    "Setup Go buffer with formatting and import organization."
    ;; Set up before-save hooks
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    
    ;; Set compile command for Go projects
    (when (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
    
    ;; Enable whitespace mode to show tabs/spaces
    (whitespace-mode 1)
    (setq-local whitespace-style '(face tabs tab-mark trailing))))

;; Go tag manipulation
(use-package go-tag
  :ensure t
  :bind (:map go-mode-map
              ("C-c t a" . go-tag-add)
              ("C-c t r" . go-tag-remove)
              ("C-c t R" . go-tag-refresh)))

;; Test generation and running
(use-package go-gen-test
  :ensure t
  :bind (:map go-mode-map
              ("C-c t g" . go-gen-test-dwim)
              ("C-c t f" . go-gen-test-for-function)))

;; Interface implementation
(use-package go-impl
  :ensure t
  :bind (:map go-mode-map
              ("C-c i" . go-impl)))

;; Enhanced test running
(use-package gotest
  :ensure t
  :bind (:map go-mode-map
              ("C-c t t" . go-test-current-test)
              ("C-c t f" . go-test-current-file)
              ("C-c t p" . go-test-current-project)
              ("C-c t b" . go-test-current-benchmark)
              ("C-c t c" . go-test-current-coverage)
              ("C-c t x" . go-run)))

;; Debugging support with dap-mode
(use-package dap-mode
  :ensure t
  :hook (go-mode . dap-mode)
  :config
  (require 'dap-dlv-go)
  :bind (:map go-mode-map
              ("C-c d d" . dap-debug)
              ("C-c d t" . dap-debug-test-at-point)
              ("C-c d l" . dap-debug-last)
              ("C-c d r" . dap-debug-recent)))

;; Go modules and workspace support
(defun go-mod-init (module-name)
  "Initialize a new Go module with MODULE-NAME."
  (interactive "sModule name: ")
  (shell-command (format "go mod init %s" module-name))
  (message "Initialized Go module: %s" module-name))

(defun go-mod-tidy ()
  "Run go mod tidy to clean up dependencies."
  (interactive)
  (shell-command "go mod tidy")
  (message "Go mod tidy completed"))

(defun go-mod-download ()
  "Download Go module dependencies."
  (interactive)
  (shell-command "go mod download")
  (message "Go mod download completed"))

(defun go-work-init ()
  "Initialize Go workspace."
  (interactive)
  (shell-command "go work init")
  (message "Go workspace initialized"))

;; Code generation helpers
(defun go-generate-stringer (type-name)
  "Generate String() method for TYPE-NAME using stringer tool."
  (interactive "sType name: ")
  (shell-command (format "go generate -run \"stringer.*%s\"" type-name))
  (revert-buffer t t))

(defun go-generate-all ()
  "Run go generate on current package."
  (interactive)
  (shell-command "go generate")
  (revert-buffer t t))

;; Performance and profiling
(defun go-build-race ()
  "Build with race detector."
  (interactive)
  (compile "go build -race"))

(defun go-test-race ()
  "Run tests with race detector."
  (interactive)
  (compile "go test -race"))

(defun go-benchmark ()
  "Run benchmarks for current package."
  (interactive)
  (compile "go test -bench=."))

(defun go-profile-cpu ()
  "Run CPU profiling."
  (interactive)
  (compile "go test -cpuprofile=cpu.prof -bench=."))

;; Enhanced key bindings (set up in mode hook)
(defun go-setup-keybindings ()
  "Setup Go-specific key bindings."
  (local-set-key (kbd "C-c m i") 'go-mod-init)
  (local-set-key (kbd "C-c m t") 'go-mod-tidy)
  (local-set-key (kbd "C-c m d") 'go-mod-download)
  (local-set-key (kbd "C-c w i") 'go-work-init)
  
  (local-set-key (kbd "C-c g s") 'go-generate-stringer)
  (local-set-key (kbd "C-c g g") 'go-generate-all)
  
  (local-set-key (kbd "C-c r b") 'go-build-race)
  (local-set-key (kbd "C-c r t") 'go-test-race)
  (local-set-key (kbd "C-c p b") 'go-benchmark)
  (local-set-key (kbd "C-c p c") 'go-profile-cpu))

(add-hook 'go-mode-hook 'go-setup-keybindings)

;; LSP enhancements for Go
(with-eval-after-load 'lsp-mode
  ;; Enable additional LSP features for Go
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedwrite . t)
                          (unusedparams . t)))
  
  ;; Improve performance
  (setq lsp-go-use-gofumpt t)  ; Use gofumpt for stricter formatting
  (setq lsp-go-goimports-local "")  ; Set your local import prefix if needed
  
  ;; Code lens settings
  (setq lsp-go-codelenses '((gc_details . t)
                           (generate . t)
                           (regenerate_cgo . t)
                           (test . t)
                           (tidy . t)
                           (upgrade_dependency . t)
                           (vendor . t))))

;; Go templates support
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . go-mode))

;; Useful Go snippets (requires yasnippet)
(with-eval-after-load 'yasnippet
  (defun go-insert-err-check ()
    "Insert Go error checking boilerplate."
    (interactive)
    (insert "if err != nil {\n\treturn err\n}"))
  
  (define-key go-mode-map (kbd "C-c e") 'go-insert-err-check))

;; Project-specific settings
(defun go-project-setup ()
  "Setup project-specific Go settings."
  (when (locate-dominating-file default-directory "go.mod")
    ;; We're in a Go module, enable some additional features
    (setq-local compile-command "go build ./...")
    (setq-local lsp-go-build-flags ["-v"])))

(add-hook 'go-mode-hook 'go-project-setup)

(provide 'go)
;;; go.el ends here