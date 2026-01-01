;;; cpp.el --- C++ language configuration with C++17+ support -*- lexical-binding: t -*-

(use-package company
  :ensure t
  :hook (c++-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package flycheck
  :ensure t
  :hook (c++-mode . flycheck-mode)
  :config
  ;; Set C++17 as minimum standard
  (setq flycheck-gcc-language-standard "c++17"
        flycheck-clang-language-standard "c++17"))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package magit
  :ensure t)

(use-package cmake-mode
  :ensure t
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

(use-package clang-format
  :ensure t
  :hook (c++-mode . (lambda ()
                      (add-hook 'before-save-hook 'clang-format-buffer nil 'local)))
  :config
  ;; Use project's .clang-format file if available
  (setq clang-format-style-option "file"))

;; Modern C++ snippets (optional but helpful)
(use-package yasnippet
  :ensure t
  :hook (c++-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; C++ LSP configuration with modern features
(add-hook 'c++-mode-hook #'lsp-deferred)
(with-eval-after-load 'lsp-mode
  ;; Find clangd executable
  (setq lsp-clients-clangd-executable 
        (or (executable-find "clangd")
            "/usr/bin/clangd"
            "/opt/homebrew/bin/clangd"))
  
  ;; Modern C++ clangd arguments
  (setq lsp-clients-clangd-args 
        '("--header-insertion=iwyu"
          "--completion-style=detailed"
          "--function-arg-placeholders"
          "--fallback-style=llvm"
          "--clang-tidy"))
  
  ;; Enable inlay hints for auto type deduction
  (setq lsp-inlay-hint-enable t))

;; CMake LSP configuration
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(cmake-mode . "cmake"))
  (when (executable-find "cmake-language-server")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "cmake-language-server")
                      :major-modes '(cmake-mode)
                      :server-id 'cmake-ls))))

(add-hook 'cmake-mode-hook #'lsp-deferred)

;; Modern C++ file extensions
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
;; Header files - let C++ mode handle them
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C++ mode configuration
(add-hook 'c++-mode-hook
          (lambda ()
            ;; Indentation settings
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)
            
            ;; Enable modern C++ font-lock for better syntax highlighting
            (setq c++-font-lock-extra-types
                  '("\\sw+_t" "\\sw+_ptr" "\\sw+_ref"
                    "std::\\sw+" "boost::\\sw+"))))

;; Custom function to set C++ standard
(defun cpp-set-standard (standard)
  "Set C++ standard for flycheck and clangd. STANDARD should be 17, 20, or 23."
  (interactive "sC++ standard (17/20/23): ")
  (let ((std-string (concat "c++" standard)))
    (setq flycheck-gcc-language-standard std-string
          flycheck-clang-language-standard std-string)
    (message "C++ standard set to C++%s" standard)))

(provide 'cpp)
;;; cpp.el ends here