(if (eq lm-in-buffer-completion 'company)
    (use-package company
      :ensure t
      :diminish
      :defer 1
      :init
      (setq lm-action-complete-ptr #'company-complete)
      (setq company-dabbrev-ignore-case t)
      (setq company-dabbrev-code-ignore-case t)    
      (setq company-keywords-ignore-case t)
      (setq company-minimum-prefix-length 3)
      (setq company-idle-delay 0.3)
      :config
      ;; (add-to-list 'company-backends '(company-capf :with company-dabbrev))
      (defun lm/company-format-margin (candidate selected)
        "Format the margin with the backend name."
        (let ((backend (company-call-backend 'annotation candidate)))
          (if backend
              (format " [%s]" backend)
            "")))
      (setq company-format-margin-function 'lm/company-format-margin)

      (global-company-mode t)))

(provide 'lm-company)
