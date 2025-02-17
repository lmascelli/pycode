(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0.3)
  (corfu-preview-current 'insert)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match 'insert)
  (text-mode-ispell-word-completion nil)
  )

(unless (display-graphic-p)
  (use-package corfu-terminal
    :init
    (custom-set-faces
     '(corfu-default ((t (:background "black")))))
    :config
    (corfu-terminal-mode)))

(provide 'lm-corfu)
