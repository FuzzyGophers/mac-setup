;;; lua.el --- Lua language configuration with LSP support

;; Core Lua mode
(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :hook ((lua-mode . lsp-deferred)
         (lua-mode . lua-setup-buffer))
  :init
  ;; Lua configuration variables
  (setq lua-indent-level 3)                    ; 3 spaces indentation (Lua standard)
  (setq lua-indent-string-contents t)          ; Indent string contents
  (setq lua-documentation-function 'browse-url) ; Documentation function
  (setq lua-default-application "lua")         ; Default Lua interpreter
  (setq lua-default-command-switches '("-i")) ; Interactive mode
  :config
  (defun lua-setup-buffer ()
    "Setup Lua buffer with formatting and development features."
    ;; Enable whitespace mode to show indentation
    (whitespace-mode 1)
    (setq-local whitespace-style '(face tabs tab-mark trailing))
    
    ;; Set up compile command for Lua scripts
    (when (buffer-file-name)
      (set (make-local-variable 'compile-command)
           (format "lua %s" (shell-quote-argument (buffer-file-name))))))
  
  ;; Key bindings for Lua development
  :bind (:map lua-mode-map
              ("C-c C-s" . lua-start-process)    ; Start Lua REPL
              ("C-c C-l" . lua-send-buffer)      ; Send buffer to REPL
              ("C-c C-f" . lua-send-defun)       ; Send function to REPL
              ("C-c C-r" . lua-send-region)      ; Send region to REPL
              ("C-c C-z" . lua-show-process-buffer) ; Switch to REPL
              ("C-c C-h" . lua-search-documentation))) ; Documentation lookup

;; Company mode for Lua completion
(use-package company-lua
  :ensure t
  :after (company lua-mode)
  :config
  (add-to-list 'company-backends 'company-lua))

;; Flycheck for Lua linting (luacheck integration)
(use-package flycheck
  :ensure t
  :hook (lua-mode . flycheck-mode)
  :config
  ;; Add luacheck checker
  (flycheck-define-checker lua-luacheck
    "A Lua syntax checker using luacheck."
    :command ("luacheck" "--formatter" "plain" "--codes" source)
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" column ": "
              "(" (id (one-or-more (not (any " )"))))
              ") " (message) line-end)
     (error line-start
            (file-name) ":" line ":" column ": "
            "(" (id (one-or-more (not (any " )"))))
            ") " (message) line-end))
    :modes lua-mode)
  
  ;; Add luacheck to flycheck checkers
  (add-to-list 'flycheck-checkers 'lua-luacheck))

;; LSP configuration for Lua
(with-eval-after-load 'lsp-mode
  ;; Lua Language Server configuration
  (setq lsp-lua-workspace-max-preload 3000)
  (setq lsp-lua-workspace-preload-file-size 1000)
  
  ;; Enable Lua diagnostics
  (setq lsp-lua-diagnostics-enable t)
  (setq lsp-lua-diagnostics-globals '("vim" "describe" "it" "before_each" "after_each"))
  
  ;; Lua runtime settings
  (setq lsp-lua-runtime-version "Lua 5.4")
  (setq lsp-lua-runtime-path '("?.lua" "?/init.lua"))
  
  ;; Completion settings
  (setq lsp-lua-completion-enable t)
  (setq lsp-lua-completion-call-snippet "Replace")
  
  ;; Hover and signature help
  (setq lsp-lua-hover-enable t)
  (setq lsp-lua-signature-help-enable t))

;; Lua project utilities
(defun lua-find-luacheck-config ()
  "Find .luacheckrc file in project."
  (locate-dominating-file default-directory ".luacheckrc"))

(defun lua-run-luacheck ()
  "Run luacheck on current file."
  (interactive)
  (if (executable-find "luacheck")
      (let ((file (buffer-file-name)))
        (if file
            (compile (format "luacheck %s" (shell-quote-argument file)))
          (message "Buffer not visiting a file")))
    (message "luacheck not found. Install via: luarocks install luacheck")))

(defun lua-run-current-file ()
  "Run current Lua file."
  (interactive)
  (if (buffer-file-name)
      (compile (format "lua %s" (shell-quote-argument (buffer-file-name))))
    (message "Buffer not visiting a file")))

(defun lua-insert-require (module)
  "Insert a require statement for MODULE."
  (interactive "sModule name: ")
  (insert (format "local %s = require('%s')" 
                  (file-name-base module) module)))

(defun lua-insert-function (name)
  "Insert a Lua function template with NAME."
  (interactive "sFunction name: ")
  (insert (format "function %s()\n\nend" name))
  (forward-line -1)
  (indent-for-tab-command))

;; Lua-specific key bindings setup
(defun lua-setup-keybindings ()
  "Setup Lua-specific key bindings."
  (local-set-key (kbd "C-c l c") 'lua-run-luacheck)
  (local-set-key (kbd "C-c l r") 'lua-run-current-file)
  (local-set-key (kbd "C-c l i") 'lua-insert-require)
  (local-set-key (kbd "C-c l f") 'lua-insert-function)
  (local-set-key (kbd "C-c l j") 'lua-set-luajit)
  (local-set-key (kbd "C-c l 5") 'lua-set-lua54)
  (local-set-key (kbd "C-c l m") 'lua-manual)
  (local-set-key (kbd "C-c l h") 'lua-quick-help))

(add-hook 'lua-mode-hook 'lua-setup-keybindings)

;; LuaJIT support
(defun lua-set-luajit ()
  "Configure buffer for LuaJIT development."
  (interactive)
  (setq-local lua-default-application "luajit")
  (setq-local lsp-lua-runtime-version "LuaJIT")
  (message "Switched to LuaJIT mode"))

(defun lua-set-lua54 ()
  "Configure buffer for Lua 5.4 development."
  (interactive)
  (setq-local lua-default-application "lua")
  (setq-local lsp-lua-runtime-version "Lua 5.4")
  (message "Switched to Lua 5.4 mode"))

;; Lua version switching (key bindings set in hook above)

;; Project detection and configuration
(defun lua-project-setup ()
  "Setup project-specific Lua settings."
  (when (lua-find-luacheck-config)
    ;; Found .luacheckrc, enable stricter checking
    (setq-local flycheck-lua-luacheck-executable "luacheck")
    (setq-local flycheck-checker 'lua-luacheck))
  
  ;; Check for common Lua project structures
  (cond
   ;; OpenResty/nginx Lua
   ((locate-dominating-file default-directory "nginx.conf")
    (setq-local lsp-lua-diagnostics-globals 
                (append lsp-lua-diagnostics-globals 
                        '("ngx" "ndk" "resty"))))
   
   ;; Awesome WM configuration
   ((string-match-p "awesome" (or (buffer-file-name) ""))
    (setq-local lsp-lua-diagnostics-globals 
                (append lsp-lua-diagnostics-globals 
                        '("awesome" "client" "screen" "mouse" "root"))))
   
   ;; Love2D game development
   ((locate-dominating-file default-directory "main.lua")
    (setq-local lsp-lua-diagnostics-globals 
                (append lsp-lua-diagnostics-globals 
                        '("love" "lg" "lf" "la" "lt" "li"))))))

(add-hook 'lua-mode-hook 'lua-project-setup)

;; Documentation and help
(defun lua-manual ()
  "Open Lua reference manual."
  (interactive)
  (browse-url "https://www.lua.org/manual/5.4/"))

(defun lua-quick-help ()
  "Show quick Lua help in minibuffer."
  (interactive)
  (message "Lua quick help: C-c C-s start REPL, C-c C-l send buffer, C-c l c luacheck, C-c l r run file"))

;; Documentation key bindings set in hook above

(provide 'lua)
;;; lua.el ends here