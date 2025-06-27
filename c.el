;; company-c-headers
(unless (package-installed-p 'company-c-headers)
  (package-refresh-contents t)
  (package-install 'company-c-headers))

(setq c-default-style "linux")
