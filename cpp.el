;;; cpp.el --- C++ language configuration

(use-package company
  :ensure t
  :hook (c++-mode . company-mode))

(use-package flycheck
  :ensure t
  :hook (c++-mode . flycheck-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package magit
  :ensure t)

(use-package cmake-mode
  :ensure t
  :mode "\\.cmake\\'")

(use-package clang-format
  :ensure t
  :hook (c++-mode . (lambda ()
                      (add-hook 'before-save-hook 'clang-format-buffer nil 'local))))

;; C++ LSP configuration
(add-hook 'c++-mode-hook #'lsp-deferred)
(with-eval-after-load 'lsp-mode
  (setq lsp-clients-clangd-executable "/usr/bin/clangd"))

;; CMake LSP configuration
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(cmake-mode . "cmake"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "cmake-language-server")
                    :major-modes '(cmake-mode)
                    :server-id 'cmake-ls)))

(add-hook 'cmake-mode-hook #'lsp-deferred)

;; C++ mode configuration
(add-hook 'c++-mode-hook
          (lambda ()
            (setq tab-width 4)))

(provide 'cpp)
;;; cpp.el ends here