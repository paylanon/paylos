;;  -------------
;; | LOOK & FEEL |
;;  -------------

(load-theme '(sokoban-theme/vanilla/sokoban) t)

(set-face-attribute 'default nil :family "ProggyCleanTTSZBP" :foundry "outline" :slant normal :weight regular :height 164 :width normal)

;;  -----------
;; | VARIABLES |
;;  -----------

(setq blink-cursor-mode nil
      menu-bar-mode nil
      tool-bar-mode nil
      visible-bell t
      custom-file "~/.emacs.d/custom.el")

;;  ----------
;; | PACKAGES |
;;  ----------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
