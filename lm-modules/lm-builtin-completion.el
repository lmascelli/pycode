;; TODO hints dabbrev-complete (f), completion-at-point-functions (v)
;; check the customize page for capf
(setq completions-format 'horizontal) ;; alternatives are `horizontal', `vertical' and `one-column'
(setq lm-action-complete-ptr #'completion-at-point)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)
(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)

(defun my/minibuffer-choose-completion (&optional no-exit no-quit)
  (interactive "P")
  (with-minibuffer-completions-window
    (let ((completion-use-base-affixes nil))
      (choose-completion nil no-exit no-quit))))

(setq lm-action-complete-ptr #'completion-at-point)
(define-key completion-in-region-mode-map (kbd "M-RET") 'my/minibuffer-choose-completion)

(provide 'lm-builtin-completion)
