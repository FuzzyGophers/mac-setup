;;; go.el --- Go language configuration

;; add go binaries to PATH
(setenv "PATH" (concat (getenv "PATH") ":~/go/bin"))
(setq exec-path (append exec-path '("~/go/bin")))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . (lambda ()
                      ;; Set up before-save hooks to format buffer and add/delete imports
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t)
                      ;; Customize compile command
                      (if (not (string-match "go" compile-command))
                          (set (make-local-variable 'compile-command)
                               "go build -v && go test -v && go vet"))
                      ;; Key bindings
                      (local-set-key (kbd "M-.") 'godef-jump)
                      (local-set-key (kbd "M-*") 'pop-tag-mark)
                      (local-set-key (kbd "C-c i") 'go-goto-imports)))))

(provide 'go)
;;; go.el ends here