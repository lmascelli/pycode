(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground))
        (dir (face-attribute 'dired-directory :foreground))
        (file (face-attribute 'default :foreground))
        (symlink (face-attribute 'dired-symlink :foreground))
        (suffix (face-attribute 'default :foreground)))
    (custom-set-faces
     `(dired-directory ((t (:foreground ,dir :weight bold))))
     `(dired-file-name ((t (:foreground ,file))))
     `(dired-symlink ((t (:foreground ,symlink))))
     `(dired-suffix ((t (:foreground ,suffix))))))
  (setq dired-dwim-target t))

(if lm-dired-sidebar (require 'lm-dired-sidebar))

(provide 'lm-dired)
