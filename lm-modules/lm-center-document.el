(defcustom lm-center-document-desired-width 90
  "Width of the centered line in lm-centered-mode"
  :type '(integer)
  :group 'lm)

(defun lm-center-document--adjust-margins ()
  ;; Reset margins first before recalculating
  (set-window-parameter nil 'min-margins nil)
  (set-window-margins nil nil)

  ;; Adjust margins if the mode is on
  (when lm-center-document-mode
    (let ((margin-width (max 0
                             (truncate
                              (/ (- (window-width)
                                    lm-center-document-desired-width)
                                 2.0)))))
      (when (> margin-width 0)
        (set-window-parameter nil 'min-margins '(0 . 0))
        (set-window-margins nil margin-width margin-width)))))

(define-minor-mode lm-center-document-mode
  "Toggle centered text layout in the current buffer."
  :lighter " Centered"
  :group 'editing
  (if lm-center-document-mode
      (add-hook 'window-configuration-change-hook #'lm-center-document--adjust-margins 'append 'local)
    (remove-hook 'window-configuration-change-hook #'lm-center-document--adjust-margins 'local))
  (lm-center-document--adjust-margins))

(provide 'lm-center-document)
