;; add go binaries to PATH
(setenv "PATH" (concat (getenv "PATH") ":~/go/bin"))
(setq exec-path (append exec-path '("~/go/bin")))

(add-hook 'go-mode-hook #'lsp-deferred)

(unless (package-installed-p 'go-mode)
  (package-refresh-contents)
  (package-install 'go-mode))

(use-package go-mode)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defun my-go-mode-hook ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; format before saving
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
    (local-set-key (kbd "C-c i") 'go-goto-imports)))
