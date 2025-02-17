(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  (which-key-setup-minibuffer))

(provide 'lm-which-key)
