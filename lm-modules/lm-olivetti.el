(use-package olivetti
  :ensure t
  :defer 1
  :init
  (lm/leader-keys
   :keymaps 'override
   :states 'normal
   "ao" '(olivetti-mode :which-key "olivetti mode"))
  :config
  (add-hook 'olivetti-mode-hook (lambda ()
                                  (display-line-numbers-mode 0)
                                  (display-fill-column-indicator-mode 0))))
