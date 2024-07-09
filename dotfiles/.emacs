(custom-set-variables
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(sokoban))
 '(custom-safe-themes
   '("c6b27497f05c869b80f7078db31a6c648dd7372e99c7a498fd962f5de0c75fcb"
     default))
 '(menu-bar-mode nil)
 '(package-selected-packages '(rg vertico))
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 '(default ((t (:family "ProggyCleanTTSZBP" :foundry "outline" :slant normal :weight regular :height 159 :width normal)))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package vertico
  :init
  (vertico-mode))

(setq default-directory "c:/Users/paylh")
