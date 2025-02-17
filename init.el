(defun lm-emacs-load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
         (expand-file-name filename
                           lm-emacs-user-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

;; void interactive function used as a placeholder
(defun lm-placeholder-f (&optional)
  (interactive)
  nil)

;; completion
(defvar lm-action-complete-ptr #'lm-placeholder-f)
(defun lm-action-complete ()
  (interactive)
  (funcall lm-action-complete-ptr))

;; buffers list
(defvar lm-action-switch-to-buffer-ptr #'(lambda () (switch-to-buffer (read-buffer "Buffer: "))))
(defun lm-action-switch-to-buffer (&rest args)
  (interactive)
  (funcall lm-action-switch-to-buffer-ptr))

;; theme toggle
(defvar lm-action-toggle-theme-ptr #'toggle-theme)
(defun lm-action-toggle-theme (&rest args)
  (interactive)
  (funcall lm-action-toggle-theme-ptr args))

(defun lm-action-insert-tilde ()                                      
  (interactive)                                 
  (insert-char (char-from-name "TILDE")))
(defun lm-action-insert-grave-accent ()                                      
  (interactive)                                 
  (insert-char (char-from-name "GRAVE ACCENT")))

(defun lm/get-conf-org-dir ()
  (interactive)
  (file-name-directory buffer-file-name))

(defun lm-action-switch-to-tab-1 ()
  (interactive)
  (tab-bar-select-tab 1))
(defun lm-action-switch-to-tab-2 ()
  (interactive)
  (tab-bar-select-tab 2))
(defun lm-action-switch-to-tab-3 ()
  (interactive)
  (tab-bar-select-tab 3))

(defun lm-open-literate-config ()
  (interactive)
  (find-file (concat lm-emacs-user-directory lm/literate-config-name)))

(defun lm-open-post-init ()
  (interactive)
  (find-file (concat lm-emacs-user-directory "post-init.el")))

(defun lm-reload-config ()
  (interactive)
  (load (concat user-emacs-directory "init.el")))

(defvar lm-held-directory nil
    "
The directory being held has default-directory. If nil no directory is being
hold. This variable is used by the `lm-toggle-hold-cwd' function.")

  (defun lm-toggle-hold-cwd ()
    (interactive)
    (setq lm-held-directory (unless lm-held-directory default-directory))
    (dolist (hook
             '(find-file-hook
               window-buffer-change-functions
               dired-mode-hook
               ))
      (add-hook hook #'(lambda () (if lm-held-directory (cd lm-held-directory))))))

(setq lm-current-eshell-counter 0)

(defun lm-make-eshell (name)
  "Create a new eshell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(defun lm-tree-dir (dir &optional prefix)
  "Print a tree of files and directories starting from DIR."
  (interactive "DDirectory: ")
  (unless prefix (setq prefix ""))
  (dolist (file (directory-files dir))
    (unless (member file '("." ".."))
      (let ((path (concat dir "/" file)))
        (insert (concat prefix (if (file-directory-p path) "+ " "- ") file "\n"))
        (when (file-directory-p path)
          (lm-tree-dir path (concat prefix "  ")))))))

(defun lm-tree-current-dir ()
  "Print a tree of the current directory."
  (interactive)
  (let ((buf (get-buffer-create "*Directory Tree*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (lm-tree-dir default-directory)
      (read-only-mode 1))
    (display-buffer buf)))

(setq lm/literate-config-name "README.org")
(setq lm/dot-dir (file-name-directory (directory-file-name lm-emacs-user-directory)))
(setq lm/sound-dir (concat lm/dot-dir "sounds/"))

(defcustom lm-tree-sitter-langs nil
  "Choose if automatically install a repo with already built grammars."
  :type '(boolean)
  :group 'lm)

(defcustom lm-input-mode 'evil
  "The keymap mode to use."
  :type '(choice
          (const :tag "evil" evil)
          (const :tag "emacs" emacs))
  :group 'lm)

(defcustom lm-key-clues 'off
  "The framework to provide clues for keymaps"
  :type '(choice
          (const :tag "which-key" which-key)
          (const :tag "off" off))
  :group 'lm)

(defcustom lm-lsp-client 'off 
  "The LSP implementation to use."
  :type '(choice
          (const :tag "eglot" eglot)
          (const :tag "lsp-mode" lsp-mode)
          (const :tag "off" off))
  :group 'lm)

(defcustom lm-capf-cape nil
  "Wheter to use or not cape package for enhance the completion at point
functions"
  :type '(boolean)
  :group 'lm)

(defcustom lm-in-buffer-completion 'builtin
  "The in-buffer completion to use."
  :type '(choice
          (const :tag "corfu" corfu)
          (const :tag "company" company)
          (const :tag "builtin" builtin))
  :group 'lm)

(defcustom lm-terminal-emulator 'off
  "The terminal emulator inside emacs"
  :type '(choice
          (const :tag "eat" eat)
          (const :tag "off" off))
  :group 'lm)

(defcustom lm-ligatures nil
  "Enables fonts ligatures."
  :type '(boolean)
  :group 'lm)

(defcustom lm-dired-sidebar nil
  "Use the dired sidebar package (C-x C-n to toggle)"
  :type '(boolean)
  :group 'lm)

(defcustom lm-exclude-dired-buffer nil
  "Disable dired buffers from buffer cycling"
  :type '(boolean)
  :group 'lm)

(defcustom lm-exclude-eshell-buffer nil
  "Disable eshell buffers from buffer cycling"
  :type '(boolean)
  :group 'lm)

;; ;;; package.el
(when (bound-and-true-p lm-emacs-package-initialize-and-refresh)
  ;; Initialize and refresh package contents again if needed
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Install use-package if necessary
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  ;; Ensure use-package is available at compile time
  (eval-when-compile
    (require 'use-package)))

(setq use-package-always-ensure nil)
(setq use-package-compute-statistics t)

;;; Minibuffer
;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package diminish
  :ensure t)

;; switch-to-buffer runs pop-to-buffer-same-window instead
(setq switch-to-buffer-obey-display-actions t)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(setq whitespace-line-column nil)  ; whitespace-mode

;; I reduced the default value of 9 to simplify the font-lock keyword,
;; aiming to improve performance. This package helps differentiate
;; nested delimiter pairs, particularly in languages with heavy use of
;; parentheses.
(setq rainbow-delimiters-max-face-count 5)

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error)

(setq truncate-string-ellipsis "…")

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))

;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward)

(setq mouse-yank-at-point t)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider` does not.
(setq window-divider-default-bottom-width 1 ;
      window-divider-default-places t
      window-divider-default-right-width 1)

(add-hook 'after-init-hook #'window-divider-mode)

;; Avoid generating backups or lockfiles to prevent creating world-readable
;; copies of files.
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files

;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default t)

;; Do not auto-disable auto-save after deleting large chunks of
;; text. The purpose of auto-save is to provide a failsafe, and
;; disabling it contradicts this objective.
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

(global-auto-revert-mode t)
;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers nil)

;; I don't like that the Buffer List reverts too quickly so it must
;; be slowed down a bit

;; (add-hook 'buffer-list-update-hook (lambda ()
;;                                    (setq-local auto-revert-interval 10)))

(setq desktop-path '((expand-file-name "desktop-saves") user-emacs-directory))

(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

;; `recentf' is an Emacs package that maintailinens a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-auto-cleanup 'mode)

;; `save-place-mode` enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)
(save-place-mode 1)

;; `savehist` is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.

(setq history-length 300)
(setq history-delete-duplicates t)
(setq extended-command-history-length 300)
(setq savehist-save-minibuffer-history t)  ;; Default
(setq savehist-file (expand-file-name "history" user-emacs-directory))
(savehist-mode t)

(setq recentf-save-file (expand-file-name "recent" user-emacs-directory))
(recentf-mode t)

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)

;; Without this, Emacs will try to resize itself to a specific column size
(setq frame-inhibit-implied-resize t)

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

;; Enables faster scrolling through unfontified regions. This may result in
;; brief periods of inaccurate syntax highlighting immediately after scrolling,
;; which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keeps screen position if the scroll command moved it vertically out of the
;; window.
(setq scroll-preserve-screen-position t)

;; Emacs 29
(when (memq 'context-menu lm-emacs-ui-features)
  (when (and (display-graphic-p) (fboundp 'context-menu-mode))
    (add-hook 'after-init-hook #'context-menu-mode)))

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends excessive time recentering the screen when the cursor
      ;; moves more than N lines past the window edges (where N is the value of
      ;; `scroll-conservatively`). This can be particularly slow in larger files
      ;; during extensive scrolling. If `scroll-conservatively` is set above
      ;; 100, the window is never automatically recentered. The default value of
      ;; 0 triggers recentering too aggressively. Setting it to 10 reduces
      ;; excessive recentering and only recenters the window when scrolling
      ;; significantly off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by preventing automatic adjustments to
      ;; `window-vscroll' for unusually long lines. Setting
      ;; `auto-window-vscroll' it to nil also resolves the issue of random
      ;; half-screen jumps during scrolling.
      auto-window-vscroll nil
      ;; Mouse
      mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 1)

;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
;; Additionally, it can cause freezing, especially on macOS, for users with
;; customized and colored cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; This controls how long Emacs will blink to show the deleted pairs with
;; `delete-pair'. A longer delay can be annoying as it causes a noticeable pause
;; after each deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Customize the behaviour of the TAB key. Bind it to:
;; - `t' Always indent the current line
;; - `'complete' Enable indentation and completion using the TAB key
(setq-default tab-always-indent 'complete)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines, which is useful for writing
;; longer comments or docstrings that span multiple lines.
(setq comment-multi-line t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Disable the obsolete practice of end-of-line spacing from the
;; typewriter era.
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or
;; more non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-auto-help t)

(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) 
                       (let ((name (buffer-name buf)))
                         (cond
                          ((eq 'dired-mode (buffer-local-value 'major-mode buf)) (not lm-exclude-dired-buffer))
                          ((string-match "eshell" name) (not lm-exclude-eshell-buffer))
                          ((string-prefix-p "*" name) nil)
                          (t t)))))

(setq custom-safe-themes t)

(unless (display-graphic-p)
  (xterm-mouse-mode))

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
 x-stretch-cursor t)

(global-visual-line-mode)

(if (display-graphic-p)
    (global-hl-line-mode))

(setq display-line-numbers-type 'relative)
;; 1. global strategy

;; (global-display-line-numbers-mode)
;; (dolist (mode '(
;;                 ;; base mode
;;                 ;; outline files
;;                 org-mode-hook
;;                 markdown-mode-hook
;;                 latex-mode-hook
;;                 ;; manuals
;;                 Info-mode-hook
;;                 ;; shell buffers
;;                 term-mode-hook
;;                 vterm-mode-hook
;;                 shell-mode-hook
;;                 eshell-mode-hook
;;                 ;; explorers
;;                 dired-mode-hook
;;                 treemacs-mode-hook
;;                 eww-mode-hook
;;                 ))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; 2. selective strategy

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Setting `display-time-default-load-average' to nil makes Emacs omit the load
;; average information from the mode line.
(setq display-time-default-load-average nil)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

(lm-emacs-load-user-init custom-file)

;; buildin packages
(require 'lm-dired)
(require 'lm-eshell)
(require 'lm-grep)
(require 'lm-org)
(require 'lm-modus-themes)

;; input mode
(cond
 ((eq lm-input-mode 'evil) (require 'lm-evil)))

(cond
 ((eq lm-key-clues 'which-key) (require 'lm-which-key)))

;; tree sitter
(if lm-tree-sitter-langs (require 'lm-tree-sitter))

;; completion
(if lm-capf-cape (require 'lm-cape))
(require 'lm-completion)

;; terminal
(cond
 ((eq lm-terminal-emulator 'eat) (require 'lm-eat)))

;; lsp
(require 'lm-lsp)

;; my packages
(use-package lm-pomodoro)
(use-package lm-center-document)

(lm-emacs-load-user-init "post-init.el")
