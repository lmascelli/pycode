(cond
 ((eq lm-in-buffer-completion 'company)
  (require 'lm-company))

 ((eq lm-in-buffer-completion 'corfu)
  (require 'lm-corfu))

 ((eq lm-in-buffer-completion 'builtin)
  (require 'lm-builtin-completion)
  ))

(provide 'lm-completion)
