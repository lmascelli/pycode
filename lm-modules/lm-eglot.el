(use-package eglot
  :ensure t
  :defer t
  :pin elpa-devel
  :init
  (setq eglot-events-buffer-config 0)
  (setq eglot-connect-timeout 90))

(provide 'lm-eglot)
