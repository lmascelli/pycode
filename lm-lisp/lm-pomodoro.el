(defun lm-pomodoro ()
  (interactive)
  (require 'org-element)
  (setq org-clock-sound (concat lm/sound-dir "bell.wav"))
  (unless (boundp 'lm/pomodoro-state)
    (setq lm/pomodoro-state 0))
  (let ((time-work "00:25:00")
        (time-pause "00:05:00"))
    (org-timer-set-timer
     (cond
      ((= (mod lm/pomodoro-state 2) 0) time-work)
      ((= (mod lm/pomodoro-state 3) 0) time-work)
      (t time-pause)))
    (setq lm/pomodoro-state (+ lm/pomodoro-state 1))))

(provide 'lm-pomodoro)
