;;  -------------
;; | LOOK & FEEL |
;;  -------------

(load-theme 'sokoban t)

(set-face-attribute 'default nil :font "ProggyCleanTTSZBP" :height 164)

(tool-bar-mode -1)
(menu-bar-mode -1)

;;  -----------
;; | VARIABLES |
;;  -----------

(setq blink-cursor-mode nil
      visible-bell t
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
  (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
