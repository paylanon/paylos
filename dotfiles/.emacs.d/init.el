;;  -------------
;; | LOOK & FEEL |
;;  -------------

(load-theme 'sokoban t)
(load-theme 'kanagawa t t)
;; (load-theme 'taylor t t)
(load-theme 'colonoscopy t t)

(set-face-attribute 'default nil :font "ProggyVector" :height 110)

(tool-bar-mode -1)
(menu-bar-mode -1)

(load-file "~/.emacs.d/splash-screen.el")
(load-file "~/.emacs.d/slang-mode.el")
(load-file "~/.emacs.d/jai-mode.el")

;;  -----------
;; | VARIABLES |
;;  -----------

(setq blink-cursor-mode nil
      visible-bell t
      display-line-numbers-type `relative
      custom-file "~/.emacs.d/custom.el"
      electric-pair-open-newline-between-pairs t)

(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6)))

(setq make-backup-files nil)
;; (setq next-line-add-newlines t)
(subword-mode 1)
(electric-pair-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . shader-mode))
(setq dired-dwim-target t)
(setq-default indent-tabs-mode nil)

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

;; == PLUG & PLAY ==

(straight-use-package 'consult)
(straight-use-package 'wgrep)
(straight-use-package 'avy)
(straight-use-package 'multiple-cursors)
(straight-use-package 'shader-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'autothemer)
(straight-use-package 'rainbow-mode)
;; (straight-use-package '(jai-mode :type git :host github :repo "krig/jai-mode"))

;; == CUSTOM CONFIG ==

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
	'((find-file flat)
	  (find-from-home flat)
	  (consult-line flat)
	  (switch-to-buffer flat)
	  (execute-extended-command flat)))
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

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package guru-mode
  :ensure t
  :config
  (guru-global-mode 1))

;; ;; DOOMEMACS which-key
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

;; slang mode
(autoload 'slang-mode "slang-mode"
 "Mode for editing slang source files")
(setq auto-mode-alist
     (append '(("\\.sl$" . slang-mode)) auto-mode-alist))

;;  ----------
;; | KEYBINDS |
;;  ----------

(global-set-key [remap isearch-forward] #'consult-line)
(global-set-key [remap imenu] #'consult-imenu)

(keymap-global-set "C-<return>" '+default/newline-below)
(keymap-global-set "C-S-<return>" '+default/newline-above)
(keymap-global-set "C-S-s" 'consult-ripgrep)
(keymap-global-set "C-x F" 'find-from-home)
(keymap-global-set "M-p" 'backward-paragraph)
(keymap-global-set "M-n" 'forward-paragraph)
(keymap-global-set "C-S-l" 'copy-whole-line)

;;; DOOMEMACS universal, non-nuclear escape
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

;; DOOMEMACS text defaults

(defalias '+default/newline #'electric-indent-just-newline)

(defun +default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))

(defun +default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

(defun +default/yank-pop ()
  "Interactively select what text to insert from the kill ring."
  (interactive)
  (call-interactively
   (cond ((fboundp 'consult-yank-pop)    #'consult-yank-pop) ;HACK see @ymarco's comment on #5013 and TODO.org in the selecturm module.
         ((fboundp 'counsel-yank-pop)    #'counsel-yank-pop)
         ((fboundp 'helm-show-kill-ring) #'helm-show-kill-ring)
         ((error "No kill-ring search backend available. Enable ivy, helm or vertico!")))))

(defun +default/yank-buffer-contents ()
  "Copy entire buffer into kill ring."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun +default/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (let ((path (abbreviate-file-name
                   (if root
                       (file-relative-name filename root)
                     filename))))
        (kill-new path)
        (if (string= path (car kill-ring))
            (message "Copied path: %s" path)
          (user-error "Couldn't copy filename in current buffer")))
    (error "Couldn't find filename in current buffer")))

(defun +default/yank-buffer-path-relative-to-project (&optional include-root)
  "Copy the current buffer's path to the kill ring.
With non-nil prefix INCLUDE-ROOT, also include the project's root."
  (interactive "P")
  (+default/yank-buffer-path
   (if include-root
       (file-name-directory (directory-file-name (doom-project-root)))
     (doom-project-root))))

(defun +default/insert-file-path (arg)
  "Insert the file name (absolute path if prefix ARG).
If `buffer-file-name' isn't set, uses `default-directory'."
  (interactive "P")
  (let ((path (or buffer-file-name default-directory)))
    (insert
     (if arg
         (abbreviate-file-name path)
       (file-name-nondirectory path)))))

(defun doom/backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((context
          (if (bound-and-true-p smartparens-mode)
              (ignore-errors (sp-get-thing))))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len current-column)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
          ;; Also, skip closing delimiters
          ((and op cl
                (string= op cl)
                (and (string= (char-to-string (or (char-before) 0)) op)
                     (setq open-len (length op)))
                (and (string= (char-to-string (or (char-after) 0)) cl)
                     (setq close-len (length cl))))
           (delete-char (- open-len))
           (delete-char close-len))

          ;; Delete up to the nearest tab column IF only whitespace between
          ;; point and bol.
          ((and (not indent-tabs-mode)
                (> tab-width 1)
                (not (bolp))
                (not (doom-point-in-string-p))
                (>= (abs (save-excursion (skip-chars-backward " \t")))
                    (setq current-column (current-column))))
           (delete-char (- (1+ (% (1- current-column) tab-width)))))

          ;; Otherwise do a regular delete
          ((delete-char -1)))))

(defun +default--delete-backward-char-a (n &optional killflag)
  "Same as `delete-backward-char', but preforms these additional checks:

+ If point is surrounded by (balanced) whitespace and a brace delimiter ({} []
  ()), delete a space on either side of the cursor.
+ If point is at BOL and surrounded by braces on adjacent lines, collapse
  newlines:
  {
  |
  } => {|}
+ Otherwise, resort to `doom/backward-delete-whitespace-to-column'.
+ Resorts to `delete-char' if n > 1"
  (interactive "p\nP")
  (or (integerp n)
      (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
              delete-active-region
              (= n 1))
         ;; If a region is active, kill or delete it.
         (if (eq delete-active-region 'kill)
             (kill-region (region-beginning) (region-end) 'region)
           (funcall region-extract-function 'delete-only)))
        ;; In Overwrite mode, maybe untabify while deleting
        ((null (or (null overwrite-mode)
                   (<= n 0)
                   (memq (char-before) '(?\t ?\n))
                   (eobp)
                   (eq (char-after) ?\n)))
         (let ((ocol (current-column)))
           (delete-char (- n) killflag)
           (save-excursion
             (insert-char ?\s (- ocol (current-column)) nil))))
        ;;
        ((= n 1)
         (cond ((or (not (modulep! +smartparens))
                    (not (bound-and-true-p smartparens-mode))
                    (and (memq (char-before) (list ?\  ?\t))
                         (save-excursion
                           (and (/= (skip-chars-backward " \t" (line-beginning-position)) 0)
                                (bolp)))))
                (doom/backward-delete-whitespace-to-column))
               ((let* ((pair (ignore-errors (sp-get-thing)))
                       (op   (plist-get pair :op))
                       (cl   (plist-get pair :cl))
                       (beg  (plist-get pair :beg))
                       (end  (plist-get pair :end)))
                  (cond ((and end beg (= end (+ beg (length op) (length cl))))
                         (delete-char (- (length op))))
                        ((doom-surrounded-p pair 'inline 'balanced)
                         (delete-char -1 killflag)
                         (delete-char 1)
                         (when (= (point) (+ (length cl) beg))
                           (sp-backward-delete-char 1)
                           (sp-insert-pair op)))
                        ((and (bolp) (doom-surrounded-p pair nil 'balanced))
                         (delete-region beg end)
                         (sp-insert-pair op)
                         t)
                        ((run-hook-with-args-until-success 'doom-delete-backward-functions))
                        ((doom/backward-delete-whitespace-to-column)))))))
        ;; Otherwise, do simple deletion.
        ((delete-char (- n) killflag))))

;; FFEmacs OCTOCAMO: language-specific themes
(defun octocamo ()
  "Load theme automatically based on current mode."
  (cond
   ((and (derived-mode-p 'jai-mode)
         (not (member 'sokoban custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'sokoban))
   ((and (derived-mode-p 'rust-mode)
         (not (member 'kanagawa custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'kanagawa))
   ((and (derived-mode-p 'conf-toml-mode)
         (not (member 'kanagawa custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'kanagawa))
   ((and (derived-mode-p 'lua-mode)
         (not (member 'doom-sourcerer custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'doom-sourcerer))
   ((and (derived-mode-p 'c-mode)
         (not (member 'colonoscopy custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'colonoscopy))
   ((and (derived-mode-p 'c++-mode)
         (not (member 'colonoscopy custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'colonoscopy))
   ((and (derived-mode-p 'shader-mode)
         (not (member 'colonoscopy custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'colonoscopy))   
   ((and (derived-mode-p 'glsl-mode)
         (not (member 'doom-rouge custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'doom-rouge))))
   ;; (t
    ;; (mapc #'disable-theme custom-enabled-themes)
    ;; (enable-theme 'sokoban))))

(add-hook 'buffer-list-update-hook 'octocamo)

;; FFEmacs FIND-FROM-HOME
(defun find-from-home ()
  "Call find-file from the home directory."
  (interactive)
  (let ((default-directory (expand-file-name "~/")))
    (call-interactively 'find-file)))

(defun display-startup-time ()
  (message
   (concat "Limit Break: " (emacs-init-time))))

(add-hook 'emacs-startup-hook #'display-startup-time)

;; FFEmacs KILL-ALL-OTHER-BUFFERS
(defun kill-all-other-buffers ()
  "Kill all buffers except current and special buffers."
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buffer (buffer-list))
      (unless (or (eq buffer current)
                  (string-prefix-p " " (buffer-name buffer))
                  (string-prefix-p "*" (buffer-name buffer)))
        (kill-buffer buffer)))
    (message "Killed all other buffers")))

;; FFEmacs COPY-WHOLE-LINE
(defun copy-whole-line ()
  "Copy current line into kill ring without marking it."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (kill-line 1)
    (yank)))

;; Start in home directory
(cd "~")


