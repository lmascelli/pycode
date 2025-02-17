(use-package doom-themes
  :config
  (setq modus-themes-headings
        '((1 . (variable-pitch light 1.4))))
  )

(use-package nerd-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)))

(provide 'lm-doom-themes)
