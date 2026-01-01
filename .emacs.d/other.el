;;; other.el --- Configuration for other languages (Terraform, YAML) -*- lexical-binding: t -*-

;; Terraform
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'"
  :hook (terraform-mode . terraform-format-on-save-mode))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(provide 'other)
;;; other.el ends here