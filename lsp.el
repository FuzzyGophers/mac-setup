(unless (package-installed-p 'lsp-mode)
  (package-refresh-contents t)
  (package-install 'lsp-mode))

(use-package lsp-mode)
