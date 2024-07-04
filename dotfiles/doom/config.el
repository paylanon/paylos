;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "ProggyCleanTTSZBP" :size 17.0)
      doom-variable-pitch-font (font-spec :family "ProggyCleanTTSZBP" :size 17.0))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-spacegrey)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-zenburn)
;; (setq doom-theme 'doom-sourcerer)
;; (setq doom-theme 'doom-gruvbox)
;; (load-theme 'kanagawa t)
;; (load-theme 'naysayer t)
;; (load-theme 'twilight t)
;; (load-theme 'darcula t)
(load! "themes/vanilla/sokoban-theme.el")
;; (load! "themes/wheatgrass-theme.el")
;; (load! "themes/emacs-automata-theme.el")
(load-theme 'doom-rouge t t)
(load-theme 'doom-badger t t)
(load-theme 'doom-sourcerer t t)
(load-theme 'sokoban t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (fringe-mode `(0 . 4))
(setq confirm-kill-emacs nil)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

;; STOP THE COMMENTS
(setq +evil-want-o/O-to-continue-comments nil)

;; STOP THE ICONS
(setq doom-modeline-icon nil)

;; LOCAL SCRIPTS
(load! "lisp/jai-mode.el")

;; PACKAGE CONFIGURATION
;; Treesitter: Add language sources
(use-package! tree-sitter
  :init
  ;; code here will run immediately
  :config
  ;; code here will run after the package is loaded
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (go "https://github.com/tree-sitter/tree-sitter-go")))
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (rustic-mode . rust-ts-mode))))

(custom-set-faces
'(evil-goggles-default-face ((t (:inherit 'org-agenda-clocking)))))

(add-hook! 'org-cdlatex-mode-hook 'electric-pair-local-mode)
(add-hook! 'org-cdlatex-mode-hook 'org-auctex-mode)
(add-hook! 'electric-pair-local-mode-hook
  (push '(?\$ . ?\$) electric-pair-pairs)
  (push '(?\* . ?\*) electric-pair-pairs))

;; DEFAULT DIRECTORY (Windows)
;; (setq-hook! '+doom-dashboard-mode-hook default-directory "C:/Users/amaka")

;; LANGUAGE-SPECIFIC THEMES

;; Function to switch themes
(defun switch-themes-based-on-mode ()
  (cond
   ((and (derived-mode-p 'jai-mode)
         (not (member 'sokoban custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'sokoban t))
   ((and (derived-mode-p 'rust-ts-mode)
         (not (member 'doom-badger custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'doom-badger t))
   ((and (derived-mode-p 'lua-mode)
         (not (member 'doom-sourcerer custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'doom-sourcerer t))
   ((and (derived-mode-p 'c++-mode)
         (not (member 'doom-sourcerer custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'doom-sourcerer t))
   ((and (derived-mode-p 'glsl-mode)
         (not (member 'doom-rouge custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'doom-rouge t))))

(add-hook 'buffer-list-update-hook 'switch-themes-based-on-mode)
