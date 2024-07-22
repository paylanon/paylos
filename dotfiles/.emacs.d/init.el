;;  -------------
;; | LOOK & FEEL |
;;  -------------

(load-theme 'sokoban t)

(set-face-attribute 'default nil :font "ProggyCleanTTSZBP" :height 164)

(tool-bar-mode -1)
(menu-bar-mode -1)

(load-file "~/.emacs.d/splash-screen.el")

;;  -----------
;; | VARIABLES |
;;  -----------

(setq blink-cursor-mode nil
      visible-bell t
      display-line-numbers-type `relative
      custom-file "~/.emacs.d/custom.el")

;;  ----------
;; | PACKAGES |
;;  ----------

;; ==== STRAIGHT.EL BOOTSTRAP ====

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ==== USE-PACKAGE LIST ====

(straight-use-package 'use-package)

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode))

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

;; DOOMEMACS which-key
(use-package which-key
  :straight t
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-setup-side-window-bottom))

;; DOOMEMACS themes
;; TODO: replace with local themes @Speed
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (load-theme 'doom-one t t)
  (load-theme 'doom-rouge t t)
  (load-theme 'doom-badger t t)
  (load-theme 'doom-sourcerer t t))

;; ==== PLUG & PLAY LIST ====

(straight-use-package 'consult)
(straight-use-package 'shader-mode)
(straight-use-package 'rainbow-mode)
(straight-use-package '(jai-mode :type git :host github :repo "krig/jai-mode"))

;;  ----------
;; | KEYBINDS |
;;  ----------

(global-set-key [remap isearch-forward] #'consult-line)

;;; DOOMEMACS universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like diff-hl and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'doom-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))

;;  -------
;; | MISC. |
;;  -------

(setq make-backup-files nil)

;; ==== CHAMELEON: LANGUAGE-SPECIFIC THEMES ====

(defun chameleon ()
  (cond
   ((and (derived-mode-p 'jai-mode)
         (not (member 'sokoban custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'sokoban))
   ((and (derived-mode-p 'rust-ts-mode)
         (not (member 'doom-badger custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'doom-badger))
   ((and (derived-mode-p 'lua-mode)
         (not (member 'doom-sourcerer custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'doom-sourcerer))
   ((and (derived-mode-p 'c++-mode)
         (not (member 'doom-sourcerer custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'doom-sourcerer))
   ((and (derived-mode-p 'glsl-mode)
         (not (member 'doom-rouge custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'doom-rouge))))

(add-hook 'buffer-list-update-hook 'chameleon)

(defun display-startup-time ()
  (message
   (concat "Limit Break: " (emacs-init-time))))

(add-hook 'emacs-startup-hook #'display-startup-time)

(defun kill-all-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; **** Personal stuff NOTE: You can delete all this ********************************

(cd "c:/Users/amaka")

;; **********************************************************************************
