(use-package yasnippet
  :ensure t
  :defer 2
  :config
  (yas-minor-mode)
  (cond
   ((eq lm-in-buffer-completion 'company)
    (progn
      ;; (add-to-list 'company-backends 'company-yasnippet)
      (global-set-key (kbd "C-c y") 'company-yasnippet)
      ))))

(use-package yasnippet-snippets
  :after yasnippet)
