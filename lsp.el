;;; lsp.el --- LSP configuration

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"))

(provide 'lsp)
;;; lsp.el ends here