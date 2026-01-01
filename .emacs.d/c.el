;;; c.el --- C language configuration with Linux kernel support -*- lexical-binding: t -*-

(use-package company-c-headers
  :ensure t)

;; Standard C style (default)
(setq c-default-style "linux")

;; Linux kernel coding style configuration
;; Based on official kernel documentation
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1) c-basic-offset)))

;; Add Linux kernel style
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-kernel"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

;; Kernel detection function
(defun c-is-kernel-source-p ()
  "Check if current buffer is in a Linux kernel source tree."
  (let ((filename (buffer-file-name)))
    (and filename
         (or (string-match-p "/linux[^/]*/" filename)
             (string-match-p "/kernel/" filename)
             (file-exists-p (concat (locate-dominating-file filename "Kconfig") "Kconfig"))
             (file-exists-p (concat (locate-dominating-file filename "Makefile") "scripts/checkpatch.pl"))))))

;; Apply kernel style function
(defun c-enable-kernel-style ()
  "Enable Linux kernel coding style for current buffer."
  (interactive)
  (setq indent-tabs-mode t)        ; Use tabs, not spaces
  (setq tab-width 8)               ; 8-character tabs
  (setq c-basic-offset 8)          ; 8-character indentation
  (setq show-trailing-whitespace t) ; Show trailing whitespace
  (c-set-style "linux-kernel")
  (setq fill-column 80)            ; 80-column limit
  (auto-fill-mode 1)               ; Enable auto-fill for comments
  (message "Linux kernel coding style enabled"))

;; Disable kernel style function
(defun c-disable-kernel-style ()
  "Disable Linux kernel coding style, revert to standard."
  (interactive)
  (setq indent-tabs-mode nil)      ; Use spaces
  (setq tab-width 4)               ; 4-character tabs
  (setq c-basic-offset 4)          ; 4-character indentation
  (setq show-trailing-whitespace nil)
  (c-set-style "linux")
  (setq fill-column 80)
  (auto-fill-mode -1)
  (message "Standard C coding style enabled"))

;; Toggle kernel style function
(defun c-toggle-kernel-style ()
  "Toggle between kernel and standard C coding styles."
  (interactive)
  (if (and (boundp 'indent-tabs-mode) indent-tabs-mode)
      (c-disable-kernel-style)
    (c-enable-kernel-style)))

;; C mode hook
(add-hook 'c-mode-hook
          (lambda ()
            ;; Auto-detect kernel source and apply style
            (when (c-is-kernel-source-p)
              (c-enable-kernel-style))
            
            ;; Key bindings for kernel style
            (local-set-key (kbd "C-c k e") 'c-enable-kernel-style)
            (local-set-key (kbd "C-c k d") 'c-disable-kernel-style)
            (local-set-key (kbd "C-c k t") 'c-toggle-kernel-style)
            
            ;; Show column numbers (helpful for 80-column limit)
            (column-number-mode 1)
            
            ;; Highlight lines over 80 characters
            (when (fboundp 'whitespace-mode)
              (setq-local whitespace-line-column 80)
              (setq-local whitespace-style '(face lines-tail))
              (whitespace-mode 1))))

;; Checkpatch integration (if available)
(defun c-run-checkpatch ()
  "Run Linux kernel checkpatch.pl on current file."
  (interactive)
  (let* ((file (buffer-file-name))
         (checkpatch (locate-dominating-file file "scripts/checkpatch.pl")))
    (if checkpatch
        (let ((cmd (concat checkpatch "scripts/checkpatch.pl --no-tree --file " file)))
          (compile cmd))
      (message "checkpatch.pl not found - not in kernel source tree?"))))

;; Add checkpatch binding
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k c") 'c-run-checkpatch)))

(provide 'c)
;;; c.el ends here