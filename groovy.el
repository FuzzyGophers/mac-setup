(unless (package-installed-p 'groovy-mode)
  (package-refresh-contents)
  (package-install 'groovy-mode))

(use-package groovy-mode)

(unless (package-installed-p 'jenkinsfile-mode)
  (package-refresh-contents)
  (package-install 'jenkinsfile-mode))

(use-package jenkinsfile-mode)
