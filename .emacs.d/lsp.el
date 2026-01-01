;;; lsp.el --- LSP configuration -*- lexical-binding: t -*-

;; Performance tuning for LSP
(setq gc-cons-threshold 100000000)           ; 100MB for GC
(setq read-process-output-max (* 1024 1024)) ; 1MB for process output

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; Performance settings
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)  ; Disable IO logging for performance

  ;; UI settings
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-code-actions-enable t)

  ;; Completion settings
  (setq lsp-completion-provider :capf)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)

  ;; Semantic highlighting
  (setq lsp-semantic-tokens-enable t)

  ;; File watching (disable for large projects if needed)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 1000))

;; LSP UI enhancements
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil))

;; Company mode for completion
(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t))

;; Flycheck for diagnostics
(use-package flycheck
  :ensure t
  :hook (lsp-mode . flycheck-mode))

(provide 'lsp)
;;; lsp.el ends here