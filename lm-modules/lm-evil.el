(defun lm-evil-personal-keymaps ()
  ;; ----------------------------- LEADER KEYMAPS ------------------------------
  (keymap-set evil-normal-state-map "SPC" lm/leader-map)
  (keymap-set evil-motion-state-map "SPC" lm/leader-map)
  (keymap-set lm/leader-map "SPC" 'execute-extended-command)
  (defvar lm/leader-map-buffer (make-sparse-keymap)
    "sub-keymap for buffer operations")
  (keymap-set lm/leader-map "b" `("+Buffer" . ,lm/leader-map-buffer))
  (keymap-set lm/leader-map-buffer "d" 'kill-buffer)
  (keymap-set lm/leader-map-buffer "s" 'save-buffer)
  (keymap-set lm/leader-map-buffer "l" #'lm-action-switch-to-buffer)
  (keymap-set lm/leader-map-buffer "L" 'list-buffers)
  (defvar lm/leader-map-quit (make-sparse-keymap)
    "sub-keymap for quit operations")
  (keymap-set lm/leader-map "q" `("+Quit" . ,lm/leader-map-quit))
  (keymap-set lm/leader-map-quit "q" 'save-buffers-kill-terminal)
  (defvar lm/leader-map-find (make-sparse-keymap)
    "sub-keymap for finding operations")
  (keymap-set lm/leader-map "f" `("+Find" . ,lm/leader-map-find))
  (keymap-set lm/leader-map-find "f" 'project-find-file)
  (keymap-set lm/leader-map-find "s" 'evil-search-forward)
  (keymap-set lm/leader-map-find "b" 'evil-search-backward)
  (keymap-set lm/leader-map-find "r" 'query-replace)
  (defvar lm/leader-map-appearence (make-sparse-keymap)
    "sub-keymap for customizing appearence operations")
  (keymap-set lm/leader-map "a" `("+Appearence" . ,lm/leader-map-appearence))
  (keymap-set lm/leader-map-appearence "c" 'customize-themes)
  (keymap-set lm/leader-map-appearence "t" 'lm-action-toggle-theme)
  (keymap-set lm/leader-map-appearence "m" 'menu-bar-mode)
  (defvar lm/leader-map-config (make-sparse-keymap)
    "sub-keymap for customizing configuration operations")
  (keymap-set lm/leader-map "c" `("+Configuration" . ,lm/leader-map-config))
  (keymap-set lm/leader-map-config "c" 'lm-open-literate-config)
  (keymap-set lm/leader-map-config "p" 'lm-open-post-init)
  (keymap-set lm/leader-map-config "r" 'lm-reload-config)
  (keymap-set lm/leader-map-config "m" 'toggle-frame-maximized)
  (defvar lm/leader-map-project (make-sparse-keymap)
    "sub-keymap for customizing project operations")
  (keymap-set lm/leader-map "p" `("+Project" . ,lm/leader-map-project))
  (keymap-set lm/leader-map-project "p" 'project-asyn-shell-command)
  (keymap-set lm/leader-map-project "d" 'lm-toggle-hold-cwd)
  (defvar lm/leader-map-special-chars (make-sparse-keymap)
    "sub-keymap for inserting special characters")
  (keymap-set lm/leader-map "i" `("+Insert" . ,lm/leader-map-special-chars))
  (keymap-set lm/leader-map-special-chars "t" 'lm-action-insert-tilde)
  (keymap-set lm/leader-map-special-chars "g" 'lm-action-insert-grave-accent)
  (defvar lm/leader-map-tools (make-sparse-keymap)
    "sub-keymap for tools")
  (keymap-set lm/leader-map "t" `("+Tools" . ,lm/leader-map-tools))
  (keymap-set lm/leader-map-tools "p" 'lm/pomodoro)
  ;; ------------------------- NORMAL STATE KEYMAPS ----------------------------
  (keymap-set evil-normal-state-map "H" 'previous-buffer)
  (keymap-set evil-normal-state-map "L" 'next-buffer)
  (keymap-set evil-normal-state-map "U" 'undo-redo)
  (keymap-set evil-normal-state-map "C-w C-h" 'evil-window-left)
  (keymap-set evil-normal-state-map "C-w C-l" 'evil-window-right)
  (keymap-set evil-normal-state-map "C-w C-j" 'evil-window-down)
  (keymap-set evil-normal-state-map "C-w C-k" 'evil-window-up)
  ;; (define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
  ;; ------------------------- VISUAL STATE KEYMAPS ----------------------------
  ;; ------------------------- INSERT STATE KEYMAPS ----------------------------
  (keymap-set evil-insert-state-map "C-g" 'evil-normal-state)
  (keymap-set evil-insert-state-map "S-<return>" #'evil-open-above)
  (keymap-set evil-insert-state-map (if (display-graphic-p) "C-SPC" "C-_") #'lm-action-complete)
  (defvar lm/insert-map (make-sparse-keymap)
    "Keymap for shortcuts in insert mode")
  (keymap-set evil-insert-state-map "C-c" lm/insert-map)
  (defvar lm/insert-map-special-chars (make-sparse-keymap)
    "sub-keymap for inserting special characters")
  (keymap-set lm/insert-map "s" `("+Special" . ,lm/insert-map-special-chars))
  (keymap-set lm/insert-map-special-chars "t" 'lm-action-insert-tilde)
  (keymap-set lm/insert-map-special-chars "g" 'lm-action-insert-grave-accent)
  ;; ----------------------------- GLOBAL KEYMAPS ------------------------------
  (keymap-set global-map "C-s" 'save-buffer)
  (keymap-set global-map "M-1" 'lm-action-switch-to-tab-1)
  (keymap-set global-map "M-2" 'lm-action-switch-to-tab-2)
  (keymap-set global-map "M-3" 'lm-action-switch-to-tab-3)
  ;; ---------------------------------------------------------------------------
  )

(use-package evil
  :ensure t
  :defer 0.1
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (unless (display-graphic-p) (setq evil-want-C-i-jump nil))
  (setq evil-undo-system 'undo-redo)
  (defvar lm/leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts")
  :config
  ;; change cursor form in terminal
  (unless (display-graphic-p)
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q"))))

  (evil-define-command nshell ()
    "Create a new eshell buffer"
    (lm-make-eshell (format "eshell-%d" lm-current-eshell-counter))
    (setq lm-current-eshell-counter (1+ lm-current-eshell-counter)))

  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :custom ((evil-collection-want-unimpaired-p nil)
           (evil-collection-setup-minibuffer t)
           (evil-collection-key-blacklist '("H" "L" "SPC")))
  :config
  (evil-collection-init)
  (lm-evil-personal-keymaps))

(provide 'lm-evil)
