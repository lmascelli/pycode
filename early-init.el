(defun lm-add-to-list (list element)
  "Add to symbol of LIST the given ELEMENT.
Simplified version of `add-to-list'."
  (set list (cons element (symbol-value list))))

(mapc
 (lambda (var)
   (lm-add-to-list var '(width . (text-pixels . 800)))
   (lm-add-to-list var '(height . (text-pixels . 900)))
   (lm-add-to-list var '(scroll-bar-width  . 10)))
 '(default-frame-alist initial-frame-alist))

(defvar lm-emacs-ui-features '(context-menu)
  "List of user interface features to disable in minimal Emacs setup.

    This variable holds a list Emacs UI features that can be enabled:
    - `context-menu`: Enables the context menu in graphical environments.
    - `tool-bar`: Enables the tool bar in graphical environments.
    - `menu-bar`: Enables the menu bar in graphical environments.
    - `dialogs`: Enables both file dialogs and dialog boxes.
    - `tooltips`: Enables tooltips.

    Each feature in the list corresponds to a specific UI component that can be
    turned on."
  )

(defvar lm-emacs-frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")
(setq frame-title-format lm-emacs-frame-title-format
      icon-title-format lm-emacs-frame-title-format)

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(unless (memq 'menu-bar lm-emacs-ui-features)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil)))


(if (display-graphic-p) (unless (daemonp)
  (unless noninteractive
    ;; Temporarily override the tool-bar-setup function to prevent it from
    ;; running during the initial stages of startup
    (advice-add #'tool-bar-setup :override #'ignore)
    (define-advice startup--load-user-init-file
        (:before (&rest _) lm-emacs-setup-toolbar)
      (advice-remove #'tool-bar-setup #'ignore)
      (tool-bar-setup)))))

(unless (memq 'tool-bar lm-emacs-ui-features)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(unless (memq 'tooltips lm-emacs-ui-features)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(unless (memq 'dialogs lm-emacs-ui-features)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil))

(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;; Ensure that some built-in (e.g., org-mode) are always up to date
(setq package-install-upgrade-built-in t)

(defvar lm-emacs-package-initialize-and-refresh t
  "Whether to automatically initialize and refresh packages.
          When set to non-nil, Emacs will automatically call `package-initialize' and
          `package-refresh-contents' to set up and update the package system.")

(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq use-package-always-ensure nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("elpa-devel" . "https://elpa.gnu.org/devel/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(customize-set-variable 'package-archive-priorities '(("elpa" . 100)
                                                      ("elpa-devel" . 101)
                                                      ("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("stable" . 70)
                                                      ("melpa"  . 0)))

(defvar lm-emacs-user-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

(setq custom-theme-directory
      (expand-file-name "themes/" lm-emacs-user-directory))

;; Add the folder of my cust lisp files and modules to the load-path
(mapc (lambda (string)
        (add-to-list 'load-path (concat lm-emacs-user-directory string)))
      '("lm-lisp" "lm-modules" "lm-languages"))

(setq custom-file (expand-file-name "custom.el" lm-emacs-user-directory))

;;; increase garbage collector when load
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar lm-emacs-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold lm-emacs-gc-cons-threshold
                  gc-cons-percentage 0.1 )))

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 512 1024))  ; 512kb

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

(defvar lm-emacs-debug nil 
  "Non-nil to enable debug.")

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  ;; (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
  ;;   (set-default-toplevel-value
  ;;    'file-name-handler-alist
  ;;    ;; Determine the state of bundled libraries using calc-loaddefs.el.
  ;;    ;; If compressed, retain the gzip handler in `file-name-handler-alist`.
  ;;    ;; If compiled or neither, omit the gzip handler during startup for
  ;;    ;; improved startup and package load time.
  ;;    (if (eval-when-compile
  ;;          (locate-file-internal "calc-loaddefs.el" load-path))
  ;;        nil
  ;;      (list (rassq 'jka-compr-handler old-value))))
  ;;   ;; Ensure the new value persists through any current let-binding.
  ;;   (set-default-toplevel-value 'file-name-handler-alist
  ;;                               file-name-handler-alist)
  ;;   ;; Remember the old value to reset it as needed.
  ;;   (add-hook 'emacs-startup-hook
  ;;             (lambda ()
  ;;               (set-default-toplevel-value
  ;;                'file-name-handler-alist
  ;;                ;; Merge instead of overwrite to preserve any changes made
  ;;                ;; since startup.
  ;;                (delete-dups (append file-name-handler-alist old-value))))
  ;;             101))

  ;;  (let ((old-file-name-handler-alist file-name-handler-alist))
  ;;    (setq vc-handled-backends nil)
  ;;    (add-hook 'emacs-startup-hook
  ;;              (lambda ()
  ;;                (setq vc-handled-backends old-vc-handled-backends))))

  (unless noninteractive
    (progn
      ;; Disable mode-line-format during init
      (defun lm-emacs--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))

      (defvar lm-emacs--default-mode-line-format mode-line-format
        "Default value of `mode-line-format'.")
      (setq-default mode-line-format nil)

      (defun lm-emacs--startup-load-user-init-file (fn &rest args)
        "Advice for startup--load-user-init-file to reset mode-line-format."
        (let (init)
          (unwind-protect
              (progn
                (apply fn args)  ; Start up as normal
                (setq init t))
            (unless init
              ;; If we don't undo inhibit-{message, redisplay} and there's an
              ;; error, we'll see nothing but a blank Emacs frame.
              (lm-emacs--reset-inhibited-vars-h))
            (unless (default-toplevel-value 'mode-line-format)
              (setq-default mode-line-format
                            lm-emacs--default-mode-line-format)))))

      (advice-add 'startup--load-user-init-file :around
                  #'lm-emacs--startup-load-user-init-file))


    ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
    ;; No second pass of case-insensitive search over auto-mode-alist.
    (setq auto-mode-case-fold nil)

    ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
    ;; dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for a modest performance boost.
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)

    ;; Give up some bidirectional functionality for slightly faster re-display.
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless lm-emacs-debug
      ;; Unset command line options irrelevant to the current OS. These options
      ;; are still processed by `command-line-1` but have no effect.
      (unless (eq system-type 'darwin)
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Some features that are not represented as packages can be found in
;; `features', but this can be inconsistent. The following enforce consistency:
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features) ; no alternative
    (push 'harfbuzz features))
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
          native-comp-deferred-compilation t  ; Obsolete since Emacs 29.1
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors
      (or lm-emacs-debug 'silent))
(setq native-comp-warning-on-missing-source lm-emacs-debug)

(setq debug-on-error lm-emacs-debug
      jka-compr-verbose lm-emacs-debug)
(setq comp-async-report-warnings-errors nil)

(setq byte-compile-warnings lm-emacs-debug)
(setq byte-compile-verbose lm-emacs-debug)

(provide 'early-init)
