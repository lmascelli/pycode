(use-package eat
  :ensure t
  :config
  (defun eat-default-shell () "pwsh")
  (setq eat-default-shell-function '(lambda () "pwsh")))

(provide 'lm-eat)
