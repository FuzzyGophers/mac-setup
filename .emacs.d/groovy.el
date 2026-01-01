;;; groovy.el --- Groovy language configuration -*- lexical-binding: t -*-

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")

(use-package jenkinsfile-mode
  :ensure t
  :mode "Jenkinsfile\\'")

(provide 'groovy)
;;; groovy.el ends here