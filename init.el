(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; mail stuff
(add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.8.5/share/emacs/site-lisp/mu/mu4e/")
(require 'mu4e)

;; ensures that each package that is configured using ~use-package~ is installed
(setq use-package-always-ensure t)

;; visual stuff
(setq-default
  ;; Prefers spaces over tabs, tabs are evil
  ;; indent-tabs-mode nil
  ;; Set width for tabs
  tab-width 4
  ;; Prefers the newest version of a file
  load-prefer-newer t
  ;; Set the full name of the user
  ;; user-full-name "xxxx xxxx"
  ;; Set the email address of the user
  ;; user-mail-address "abcxyz@gmail.com"
  ;; The confirmation string when exiting Emacs
  confirm-kill-emacs 'y-or-n-p
  fill-column 80
)

;; mac os setup for cmd key
;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(setq
 whitespace-style
 '(face
   trailing
   lines-tail
   space-before-tab
   space-after-tab
   newline
   identation
   empty
   whitespace-line-column 80
   )
 )

;; wrap lines at 80 chars
(add-hook 'text-mode-hook #'auto-fill-mode)

;; properly delete tabs
(setq backward-delete-char-untabify-method 'hungry)

;; Enable the following minor modes globally
;; Show the column number
(column-number-mode 1)
;; Display time in the mode-line
(display-time-mode 1)
;; Replace yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; Highlight current line
(global-hl-line-mode)
;; Show the matching set of parentheses
(show-paren-mode 1)
;; Display battery percentage
(display-battery-mode 1)

;; sane backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; mail config
;; (load "~/.emacs.d/mail.el")

;; go config
(load "~/.emacs.d/go.el")

;; c config
(load "~/.emacs.d/c.el")

;; other languages
;; terraform and YAML
(load "~/.emacs.d/other.el")

;; sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init

:config

(push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
(push 'rotate-windows dired-sidebar-toggle-hidden-commands)

(setq dired-sidebar-subtree-line-prefix "__")
(setq dired-sidebar-theme 'vscode)
(setq dired-sidebar-use-term-integration t)
(setq dired-sidebar-use-custom-font t))


;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'dracula t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company-c-headers go-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
