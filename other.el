;; Terraform
;; https://github.com/emacsorphanage/terraform-mode

(unless (package-installed-p 'terraform-mode)
  (package-refresh-contents)
  (package-install 'terraform-mode))

(use-package terraform-mode)

(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; YAML

(unless (package-installed-p 'yaml-mode)
  (package-refresh-contents)
  (package-install 'yaml-mode))

(use-package yaml-mode)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
