;;; treesitter.el --- Tree-sitter configuration for modern syntax highlighting -*- lexical-binding: t -*-

;; Tree-sitter provides faster, more accurate syntax highlighting
;; Requires Emacs 29+ with tree-sitter support

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  ;; Auto-install tree-sitter grammars
  (use-package treesit-auto
    :ensure t
    :config
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode))

  ;; Remap major modes to tree-sitter versions when available
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (rust-mode . rust-ts-mode)
          (go-mode . go-ts-mode)
          (bash-mode . bash-ts-mode))))

(provide 'treesitter)
;;; treesitter.el ends here
