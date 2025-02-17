(cond
 ((eq lm-lsp-client 'lsp-mode) (require 'lm-lsp-mode))

 ((eq lm-lsp-client 'eglot) (require 'lm-eglot))

 (t nil))

(provide 'lm-lsp)
