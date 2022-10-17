(setq
 mu4e-headers-results-limit 2000
 mu4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"
 mu4e-update-interval (* 10 60)
 mu4e-maildir       "~/Mail"   ;; top-level Maildir
 user-mail-address "bman@gentoo.org"
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash")

;; this setting allows to re-sync and re-index mail
;; by pressing U
(setq mu4e-get-mail-command  "mbsync -a")

;; configure sending of mail
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/opt/homebrew/Cellar/msmtp/1.8.20/bin/msmtp")
