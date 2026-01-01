;;; typescript.el --- TypeScript/JavaScript configuration with LSP support -*- lexical-binding: t -*-

;; TypeScript mode with LSP integration
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook ((typescript-mode . lsp-deferred)
         (typescript-mode . typescript-setup-buffer))
  :init
  (setq typescript-indent-level 2))

;; JavaScript mode hooks (js-mode is built-in)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'typescript-setup-buffer)

;; Handle additional JS file extensions
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))

;; Buffer setup function
(defun typescript-setup-buffer ()
  "Setup TypeScript/JavaScript buffer with formatting and development features."
  (whitespace-mode 1)
  (setq-local whitespace-style '(face tabs tab-mark trailing))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local js-indent-level 2)

  ;; Set up Prettier format-on-save
  (add-hook 'after-save-hook #'typescript-prettier-after-save nil t)

  ;; Set compile command for TypeScript projects
  (when (buffer-file-name)
    (cond
     ((typescript-find-package-json)
      (set (make-local-variable 'compile-command)
           (if (file-exists-p (concat (typescript-find-package-json) "yarn.lock"))
               "yarn build"
             "npm run build")))
     ((typescript-find-tsconfig)
      (set (make-local-variable 'compile-command) "npx tsc --noEmit")))))

;; Prettier formatting (following markdown.el pattern)
(defun typescript-format-with-prettier ()
  "Format the current TypeScript/JavaScript buffer using Prettier."
  (interactive)
  (when-let* ((prettier (executable-find "prettier"))
              (file (buffer-file-name)))
    (when (typescript-buffer-p)
      (let ((output-buffer "*Prettier Output*"))
        (with-current-buffer (get-buffer-create output-buffer)
          (erase-buffer))
        (let ((exit-code (call-process prettier nil output-buffer nil "--write" (shell-quote-argument file))))
          (if (zerop exit-code)
              (revert-buffer t t t)
            (message "Prettier failed with exit code %d" exit-code)))))))

(defun typescript-prettier-after-save ()
  "Format with prettier after saving."
  (when (typescript-buffer-p)
    (typescript-format-with-prettier)))

(defun typescript-buffer-p ()
  "Check if current buffer is a TypeScript/JavaScript buffer."
  (or (derived-mode-p 'typescript-mode 'js-mode)))

;; Project detection utilities
(defun typescript-find-package-json ()
  "Find the nearest package.json directory."
  (when (buffer-file-name)
    (locate-dominating-file (buffer-file-name) "package.json")))

(defun typescript-find-tsconfig ()
  "Find the nearest tsconfig.json directory."
  (when (buffer-file-name)
    (or (locate-dominating-file (buffer-file-name) "tsconfig.json")
        (locate-dominating-file (buffer-file-name) "jsconfig.json"))))

(defun typescript-find-eslint-config ()
  "Find the nearest ESLint configuration file."
  (when (buffer-file-name)
    (or (locate-dominating-file (buffer-file-name) ".eslintrc.js")
        (locate-dominating-file (buffer-file-name) ".eslintrc.json")
        (locate-dominating-file (buffer-file-name) ".eslintrc.yml")
        (locate-dominating-file (buffer-file-name) "eslint.config.js")
        (locate-dominating-file (buffer-file-name) "eslint.config.mjs"))))

;; LSP configuration for TypeScript
(with-eval-after-load 'lsp-mode
  (require 'lsp-eslint nil t)

  ;; TypeScript-specific LSP settings
  (setq lsp-typescript-suggest-complete-function-calls t)
  (setq lsp-typescript-preferences-import-module-specifier "relative")
  (setq lsp-javascript-suggest-complete-function-calls t)

  ;; Enable code lens
  (setq lsp-typescript-references-code-lens-enabled t)
  (setq lsp-typescript-implementations-code-lens-enabled t))

;; Utility functions
(defun typescript-run-tsc ()
  "Run TypeScript compiler on current project."
  (interactive)
  (let ((root (typescript-find-tsconfig)))
    (if root
        (let ((default-directory root))
          (compile "npx tsc --noEmit"))
      (message "No tsconfig.json found"))))

(defun typescript-run-eslint-fix ()
  "Run ESLint --fix on current file."
  (interactive)
  (if (typescript-find-eslint-config)
      (let ((file (buffer-file-name)))
        (if file
            (progn
              (shell-command (format "npx eslint --fix %s" (shell-quote-argument file)))
              (revert-buffer t t t)
              (message "ESLint fix completed"))
          (message "Buffer not visiting a file")))
    (message "No ESLint configuration found")))

(defun typescript-npm-install ()
  "Run npm install or yarn install in project root."
  (interactive)
  (let ((root (typescript-find-package-json)))
    (if root
        (let ((default-directory root))
          (if (file-exists-p (concat root "yarn.lock"))
              (compile "yarn install")
            (compile "npm install")))
      (message "No package.json found"))))

(defun typescript-npm-run (script)
  "Run npm/yarn script SCRIPT."
  (interactive "sScript name: ")
  (let ((root (typescript-find-package-json)))
    (if root
        (let ((default-directory root))
          (compile (if (file-exists-p (concat root "yarn.lock"))
                       (format "yarn %s" script)
                     (format "npm run %s" script))))
      (message "No package.json found"))))

(defun typescript-npm-test ()
  "Run npm test or yarn test."
  (interactive)
  (typescript-npm-run "test"))

;; Key bindings setup
(defun typescript-setup-keybindings ()
  "Setup TypeScript-specific key bindings."
  ;; Formatting
  (local-set-key (kbd "C-c f p") 'typescript-format-with-prettier)
  (local-set-key (kbd "C-c f e") 'typescript-run-eslint-fix)

  ;; TypeScript compiler
  (local-set-key (kbd "C-c t c") 'typescript-run-tsc)

  ;; NPM/Yarn
  (local-set-key (kbd "C-c n i") 'typescript-npm-install)
  (local-set-key (kbd "C-c n r") 'typescript-npm-run)
  (local-set-key (kbd "C-c n t") 'typescript-npm-test))

(add-hook 'typescript-mode-hook 'typescript-setup-keybindings)
(add-hook 'js-mode-hook 'typescript-setup-keybindings)

(provide 'typescript)
;;; typescript.el ends here
