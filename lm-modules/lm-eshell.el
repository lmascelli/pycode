(setq eshell-hist-ignoredups 'erase)
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
(provide 'lm-eshell)
