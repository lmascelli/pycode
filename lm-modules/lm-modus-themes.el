(use-package modus-themes
  :ensure t
  :demand t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  :config
  (setq lm-action-toggle-theme-ptr #'(lambda (&rest r) (modus-themes-toggle))))

(provide 'lm-modus-themes)
