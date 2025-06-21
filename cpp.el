(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg '(lsp-mode company flycheck projectile magit))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'lsp-mode)
(setq lsp-clients-clangd-executable "/opt/homebrew/opt/llvm/bin/clangd")
(add-hook 'c++-mode-hook 'lsp)
(require 'company)
(add-hook 'c++-mode-hook 'company-mode)
(require 'flycheck)
(add-hook 'c++-mode-hook 'flycheck-mode)
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; CMake support
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'cmake-mode)
  (package-install 'cmake-mode))

(require 'lsp-mode)
(add-to-list 'lsp-language-id-configuration '(cmake-mode . "cmake"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "cmake-language-server")
                  :major-modes '(cmake-mode)
                  :server-id 'cmake-ls))
(add-hook 'cmake-mode-hook #'lsp)
