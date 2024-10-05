;;; kanagawa-theme.el --- An elegant theme inspired by The Great Wave off Kanagawa by Katsushika Hokusa -*- lexical-binding: t -*-

;; Copyright (C) 2023 Mikael Konradsson
;; Copyright (C) 2023-2024 Meritamen <meritamen@sdf.org>

;; Author: Meritamen <meritamen@sdf.org>
;; URL: https://github.com/meritamen/emacs-kanagawa-theme
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Created: 16 September 2023
;; Keywords: themes faces

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Kanagawa is a theme inspired by the colors of the famous painting
;; The Great Wave off Kanagawa by Katsushika Hokusa.
;; Original theme created by rebelot see: https://github.com/rebelot/kanagawa.nvim
;; Edited by paylanon to include and default to Dragon.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

;;; Customizations:

(defgroup kanagawa-theme nil
  "Kanagawa-theme options."
  :group 'faces)

(defcustom kanagawa-theme-comment-italic t
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'kanagawa-theme)

(defcustom kanagawa-theme-keyword-italic t
  "Enable italics for keywords."
  :type 'boolean
  :group 'kanagawa-theme)

(defcustom kanagawa-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'kanagawa-theme)

(defcustom kanagawa-theme-org-bold t
  "Inherit text bold for org headings"
  :type 'boolean
  :group 'kanagawa-theme)

(defcustom kanagawa-theme-org-priority-bold t
  "Inherit text bold for priority items in agenda view"
  :type 'boolean
  :group 'kanagawa-theme)

(defcustom kanagawa-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'kanagawa-theme)

(defcustom kanagawa-theme-underline-parens t
  "If non-nil, underline matching parens when using `show-paren-mode' or similar."
  :type 'boolean
  :group 'kanagawa-theme)

(defun true-color-p ()
  (or (display-graphic-p)
      (= (tty-display-color-cells) 16777216)))

(deftheme kanagawa "An elegant theme inspired by The Great Wave off Kanagawa by Katsushika Hokusa")

(defconst kanagawa-dark-palette
  `((fuji-white       ,(if (true-color-p) "#DCD7BA" "#ffffff"))
    (old-white        ,(if (true-color-p) "#C8C093" "#ffffff"))
    (sumi-ink-0       ,(if (true-color-p) "#16161D" "#000000"))
    (sumi-ink-1b      ,(if (true-color-p) "#1f1f28" "#000000"))
    (sumi-ink-1       ,(if (true-color-p) "#1F1F28" "#080808"))
    (sumi-ink-2       ,(if (true-color-p) "#2A2A37" "#121212"))
    (sumi-ink-3       ,(if (true-color-p) "#363646" "#303030"))
    (sumi-ink-4       ,(if (true-color-p) "#54546D" "#303030"))
    (wave-blue-1      ,(if (true-color-p) "#223249" "#4e4e4e"))
    (wave-blue-2      ,(if (true-color-p) "#2D4F67" "#585858"))
    (wave-aqua-1      ,(if (true-color-p) "#6A9589" "#6a9589"))
    (wave-aqua-2      ,(if (true-color-p) "#7AA89F" "#717C7C"))
    (winter-green     ,(if (true-color-p) "#2B3328" "#585858"))
    (winter-yellow    ,(if (true-color-p) "#49443C" "#585858"))
    (winter-red       ,(if (true-color-p) "#43242B" "#585858"))
    (winter-blue      ,(if (true-color-p) "#252535" "#585858"))
    (autumn-green     ,(if (true-color-p) "#76946A" "#585858"))
    (autumn-red       ,(if (true-color-p) "#C34043" "#585858"))
    (autumn-yellow    ,(if (true-color-p) "#DCA561" "#585858"))
    (samurai-red      ,(if (true-color-p) "#E82424" "#585858"))
    (ronin-yellow     ,(if (true-color-p) "#FF9E3B" "#585858"))
    (dragon-blue      ,(if (true-color-p) "#658594" "#658594"))
    (fuji-gray        ,(if (true-color-p) "#727169" "#717C7C"))
    (spring-violet-1  ,(if (true-color-p) "#938AA9" "#717C7C"))
    (oni-violet       ,(if (true-color-p) "#957FB8" "#717C7C"))
    (crystal-blue     ,(if (true-color-p) "#7E9CD8" "#717C7C"))
    (spring-violet-2  ,(if (true-color-p) "#9CABCA" "#717C7C"))
    (spring-blue      ,(if (true-color-p) "#7FB4CA" "#717C7C"))
    (light-blue       ,(if (true-color-p) "#A3D4D5" "#717C7C"))
    (spring-green     ,(if (true-color-p) "#98BB6C" "#717C7C"))
    (boat-yellow-1    ,(if (true-color-p) "#938056" "#717C7C"))
    (boat-yellow-2    ,(if (true-color-p) "#C0A36E" "#717C7C"))
    (carp-yellow      ,(if (true-color-p) "#E6C384" "#717C7C"))
    (sakura-pink      ,(if (true-color-p) "#D27E99" "#717C7C"))
    (wave-red         ,(if (true-color-p) "#E46876" "#717C7C"))
    (peach-red        ,(if (true-color-p) "#FF5D62" "#717C7C"))
    (surimi-orange    ,(if (true-color-p) "#FFA066" "#717C7C"))
    (katana-gray      ,(if (true-color-p) "#717C7C" "#717C7C"))
    (comet            ,(if (true-color-p) "#54536D" "#4e4e4e"))

    ;; @EDITED Dragon colors
    (dragon-black-0   ,(if (true-color-p) "#0D0C0C" "#4e4e4e"))
    (dragon-black-1   ,(if (true-color-p) "#12120F" "#4e4e4e"))
    (dragon-black-2   ,(if (true-color-p) "#1D1C19" "#4e4e4e"))
    (dragon-black-3   ,(if (true-color-p) "#181616" "#4e4e4e"))
    (dragon-black-4   ,(if (true-color-p) "#282727" "#4e4e4e"))
    (dragon-black-5   ,(if (true-color-p) "#393836" "#4e4e4e"))
    (dragon-black-6   ,(if (true-color-p) "#625E5A" "#4e4e4e"))
    (dragon-white     ,(if (true-color-p) "#C5C9C5" "#4e4e4e"))
    (dragon-green     ,(if (true-color-p) "#87A987" "#4e4e4e"))
    (dragon-green-2   ,(if (true-color-p) "#8A9A7B" "#4e4e4e"))
    (dragon-pink      ,(if (true-color-p) "#A292A3" "#4e4e4e"))
    (dragon-orange    ,(if (true-color-p) "#B6927B" "#4e4e4e"))
    (dragon-orange-2  ,(if (true-color-p) "#B98D7B" "#4e4e4e"))
    (dragon-gray      ,(if (true-color-p) "#A6A69C" "#4e4e4e"))
    (dragon-gray-2    ,(if (true-color-p) "#9E9B93" "#4e4e4e"))
    (dragon-gray-3    ,(if (true-color-p) "#7A8382" "#4e4e4e"))
    (dragon-blue-2    ,(if (true-color-p) "#8BA4B0" "#4e4e4e"))
    (dragon-violet    ,(if (true-color-p) "#8992A7" "#4e4e4e"))
    (dragon-red       ,(if (true-color-p) "#C4746E" "#4e4e4e"))
    (dragon-aqua      ,(if (true-color-p) "#8EA4A2" "#4e4e4e"))
    (dragon-ash       ,(if (true-color-p) "#737C73" "#4e4e4e"))
    (dragon-teal      ,(if (true-color-p) "#949FB5" "#4e4e4e"))
    (dragon-yellow    ,(if (true-color-p) "#C4B28A" "#4e4e4e"))))

(defmacro define-kanagawa-dark-theme (theme &rest faces)
  `(let ((class '((class color) (min-colors 89)))
         ,@kanagawa-dark-palette)
     (custom-theme-set-faces
      ,theme
      ,@faces)))

(define-kanagawa-dark-theme
 'kanagawa
 `(default                                       ((,class (:background ,dragon-black-1 :foreground ,dragon-white))))
 `(border                                        ((,class (:background ,dragon-black-1 :foreground ,dragon-black-0))))
 `(button                                        ((,class (:foreground ,dragon-aqua))))
 `(child-frame                                   ((,class (:background ,dragon-black-0 :foreground ,dragon-black-0))))
 `(child-frame-border                            ((,class (:background ,dragon-black-0 :foreground ,dragon-black-0))))
 `(cursor                                        ((,class (:background ,dragon-blue-2 :foreground ,dragon-black-0 :weight bold))))
 `(error                                         ((,class (:foreground ,dragon-red))))
 `(fringe                                        ((,class (:foreground ,dragon-black-3))))
 `(glyph-face                                    ((,class (:background ,dragon-black-4))))
 `(glyphless-char                                ((,class (:foreground ,dragon-black-4))))
 `(header-line                                   ((,class (:background ,dragon-black-0))))
 `(highlight                                     ((,class (:background ,dragon-black-4 :foreground ,dragon-violet))))
 `(hl-line                                       ((,class (:background ,dragon-black-2))))
 `(homoglyph                                     ((,class (:foreground ,dragon-blue-2))))
 `(internal-border                               ((,class (:background ,dragon-black-1))))
 `(line-number                                   ((,class (:foreground ,dragon-black-4))))
 `(line-number-current-line                      ((,class (:foreground ,dragon-violet :background ,dragon-black-2 :weight bold))))
 `(lv-separator                                  ((,class (:foreground ,dragon-blue-2 :background ,dragon-black-2))))
 `(match                                         ((,class (:background ,dragon-yellow :foreground ,dragon-black-0))))
 `(menu                                          ((,class (:background ,dragon-black-0 :foreground ,dragon-white))))
 `(mode-line                                     ((,class (:background ,dragon-black-0 :foreground ,dragon-white :weight bold))))
 `(mode-line-inactive                            ((,class (:background unspecified :foreground ,dragon-black-4
								       :box (:line-width 1 :color ,dragon-black-2)))))
 `(mode-line-active                              ((,class (:background ,dragon-black-0 :foreground ,old-white
								       :box (:line-width 1 :color ,dragon-black-3)))))
 `(mode-line-highlight                           ((,class (:foreground ,dragon-yellow))))
 `(mode-line-buffer-id                           ((,class (:foreground ,dragon-aqua :weight bold))))
 `(numbers                                       ((,class (:background ,dragon-pink))))
 `(region                                        ((,class (:background ,dragon-blue))))
 `(separator-line                                ((,class (:background ,dragon-black-0))))
 `(fill-column-indicator                         ((,class (:background ,dragon-black-1 :foreground ,dragon-black-0))))
 `(shadow                                        ((,class (:foreground ,dragon-ash))))
 `(success                                       ((,class (:foreground ,dragon-aqua))))
 `(vertical-border                               ((,class (:foreground ,dragon-black-4))))
 `(warning                                       ((,class (:foreground ,dragon-orange-2))))
 `(window-border                                 ((,class (:background ,dragon-black-1))))
 `(window-divider                                ((,class (:foreground ,dragon-black-2))))
 `(hi-yellow                                     ((,class (:background ,dragon-yellow :foreground ,dragon-black-1))))

 ;; Font lock
 `(font-lock-type-face                           ((,class (:foreground ,dragon-aqua))))
 `(font-lock-regexp-grouping-backslash           ((,class (:foreground ,boat-yellow-1))))
 `(font-lock-keyword-face                        ((,class (:foreground ,dragon-violet :slant ,(if kanagawa-theme-keyword-italic 'italic 'normal)))))
 `(font-lock-warning-face                        ((,class (:foreground ,dragon-orange-2))))
 `(font-lock-string-face                         ((,class (:foreground ,dragon-white :slant italic))))
 `(font-lock-builtin-face                        ((,class (:foreground ,spring-blue))))
 `(font-lock-reference-face                      ((,class (:foreground ,peach-red))))
 `(font-lock-constant-face                       ((,class (:foreground ,old-white))))
 `(font-lock-function-name-face                  ((,class (:foreground ,dragon-gray-2))))
 `(font-lock-variable-name-face                  ((,class (:foreground ,dragon-blue))))
 `(font-lock-negation-char-face                  ((,class (:foreground ,dragon-red))))
 `(font-lock-comment-face                        ((,class (:foreground ,dragon-gray :slant ,(if kanagawa-theme-keyword-italic 'italic 'normal)))))
 `(font-lock-comment-delimiter-face              ((,class (:foreground ,dragon-gray :slant ,(if kanagawa-theme-keyword-italic 'italic 'normal)))))
 `(font-lock-doc-face                            ((,class (:foreground ,dragon-black-4))))
 `(font-lock-doc-markup-face                     ((,class (:foreground ,dragon-black-4))))
 `(font-lock-preprocessor-face                   ((,class (:foreground ,boat-yellow-1))))
 `(elisp-shorthand-font-lock-face                ((,class (:foreground ,dragon-white))))
 `(info-xref                                     ((,class (:foreground ,dragon-yellow))))
 `(minibuffer-prompt-end                         ((,class (:foreground ,dragon-red :background ,winter-red))))
 `(minibuffer-prompt                             ((,class (:foreground ,dragon-yellow :background ,winter-yellow))))
 `(epa-mark                                      ((,class (:foreground ,dragon-red))))
 `(dired-mark                                    ((,class (:foreground ,dragon-red))))
 `(trailing-whitespace                           ((,class (:background ,dragon-black-4))))

 ;; Battery colors
 `(doom-modeline-battery-critical                ((,class (:foreground ,dragon-red))))
 `(doom-modeline-battery-warning                 ((,class (:foreground ,dragon-green))))
 `(doom-modeline-battery-charging                ((,class (:foreground ,dragon-gray))))
 `(doom-modeline-battery-error                   ((,class (:foreground ,dragon-red))))
 `(doom-modeline-battery-normal                  ((,class (:foreground ,dragon-violet))))
 `(doom-modeline-battery-full                    ((,class (:foreground ,dragon-aqua))))

 ;; Doom visual state
 `(doom-modeline-evil-motion-state               ((,class (:foreground ,light-blue))))
 `(doom-modeline-evil-emacs-state                ((,class (:foreground ,crystal-blue))))
 `(doom-modeline-evil-insert-state               ((,class (:foreground ,peach-red))))
 `(doom-modeline-evil-normal-state               ((,class (:foreground ,light-blue))))
 `(doom-modeline-evil-visual-state               ((,class (:foreground ,spring-green))))
 `(doom-modeline-evil-replace-state              ((,class (:foreground ,dragon-orange-2))))
 `(doom-modeline-evil-operator-state             ((,class (:foreground ,crystal-blue))))


 `(doom-modeline-project-dir                     ((,class (:weight bold :foreground ,wave-aqua-2))))
 `(doom-modeline-buffer-path                     ((,class (:inherit bold :foreground ,wave-aqua-2))))
 `(doom-modeline-buffer-file                     ((,class (:inherit bold :foreground ,dragon-violet))))
 `(doom-modeline-buffer-modified                 ((,class (:inherit bold :foreground ,dragon-yellow))))
 `(doom-modeline-error                           ((,class (:background ,peach-red))))
 `(doom-modeline-buffer-major-mode               ((,class (:foreground ,wave-aqua-2 :weight bold))))
 `(doom-modeline-info                            ((,class (:weight bold :foreground ,light-blue))))
 `(doom-modeline-project-dir                     ((,class (:weight bold :foreground ,surimi-orange))))
 `(doom-modeline-bar                             ((,class (:weight bold :background ,spring-violet-1))))
 `(doom-modeline-panel                           ((,class (:inherit bold :background ,boat-yellow-2 :foreground ,dragon-black-2))))
 `(doom-themes-visual-bell                       ((,class (:background ,autumn-red))))

 ;; elfeed
 `(elfeed-search-feed-face                       ((,class (:foreground ,spring-violet-1))))
 `(elfeed-search-tag-face                        ((,class (:foreground ,wave-aqua-2))))

 ;; message colors
 `(message-header-name                           ((,class (:foreground ,dragon-black-4))))
 `(message-header-other                          ((,class (:foreground ,surimi-orange))))
 `(message-header-subject                        ((,class (:foreground ,dragon-yellow))))
 `(message-header-to                             ((,class (:foreground ,old-white))))
 `(message-header-cc                             ((,class (:foreground ,wave-aqua-2))))
 `(message-header-xheader                        ((,class (:foreground ,old-white))))
 `(custom-link                                   ((,class (:foreground ,crystal-blue))))
 `(link                                          ((,class (:foreground ,crystal-blue))))

 ;; markdown
 `(markdown-header-face-1                       ((,class (:inherit bold :foreground ,peach-red))))
 `(markdown-header-face-2                       ((,class (:inherit bold :foreground ,spring-violet-2))))
 `(markdown-header-face-3                       ((,class (:foreground ,boat-yellow-1))))
 `(markdown-header-face-4                       ((,class (:foreground ,dragon-white))))
 `(markdown-header-face-5                       ((,class (:foreground ,dragon-white))))
 `(markdown-header-face-6                       ((,class (:foreground ,dragon-yellow))))

 ;; org-mode
 `(org-done                                      ((,class (:foreground ,dragon-blue))))
 `(org-code                                      ((,class (:background ,dragon-black-0))))
 `(org-meta-line                                 ((,class (:background ,winter-green :foreground ,spring-green))))
 `(org-block                                     ((,class (:background ,winter-blue :foreground ,dragon-white))))
 `(org-block-begin-line                          ((,class (:background ,winter-blue :foreground ,spring-blue))))
 `(org-block-end-line                            ((,class (:background ,winter-blue :foreground ,spring-blue))))
 `(org-headline-done                             ((,class (:foreground ,dragon-blue :strike-through t))))
 `(org-todo                                      ((,class (:foreground ,surimi-orange :weight bold))))
 `(org-headline-todo                             ((,class (:foreground ,dragon-black-2))))
 `(org-upcoming-deadline                         ((,class (:foreground ,peach-red))))
 `(org-footnote                                  ((,class (:foreground ,wave-aqua-2))))
 `(org-indent                                    ((,class (:background ,dragon-black-1 :foreground ,dragon-black-1))))
 `(org-hide                                      ((,class (:background ,dragon-black-1 :foreground ,dragon-black-1))))
 `(org-date                                      ((,class (:foreground ,wave-blue-2))))
 `(org-ellipsis                                  ((,class (:foreground ,wave-blue-2 :weight bold))))
 `(org-level-1                                   ((,class (:inherit bold :foreground ,peach-red :height ,(if kanagawa-theme-org-height 1.3 1.0) :weight ,(if kanagawa-theme-org-bold 'unspecified 'normal)))))
 `(org-level-2                                   ((,class (:inherit bold :foreground ,spring-violet-2 :height ,(if kanagawa-theme-org-height 1.2 1.0) :weight ,(if kanagawa-theme-org-bold 'unspecified 'normal)))))
 `(org-level-3                                   ((,class (:foreground ,boat-yellow-1 :height ,(if kanagawa-theme-org-height 1.1 1.0)))))
 `(org-level-4                                   ((,class (:foreground ,dragon-white))))
 `(org-level-5                                   ((,class (:foreground ,dragon-white))))
 `(org-level-6                                   ((,class (:foreground ,dragon-yellow))))
 `(org-level-7                                   ((,class (:foreground ,surimi-orange))))
 `(org-level-8                                   ((,class (:foreground ,spring-green))))
 `(org-priority                                  ((,class (:foreground ,peach-red :inherit bold :weight ,(if kanagawa-theme-org-priority-bold 'unspecified 'normal)))))

 ;; imenu
 `(imenu-list-entry-face                         ((,class (:foreground ,dragon-white))))
 `(imenu-list-entry-face-0                       ((,class (:foreground ,peach-red))))
 `(imenu-list-entry-face-1                       ((,class (:foreground ,spring-violet-2))))
 `(imenu-list-entry-face-2                       ((,class (:foreground ,boat-yellow-1))))
 `(imenu-list-entry-face-3                       ((,class (:foreground ,dragon-white))))
 `(imenu-list-entry-subalist-face-0              ((,class (:foreground ,peach-red))))
 `(imenu-list-entry-subalist-face-1              ((,class (:foreground ,spring-violet-2))))
 `(imenu-list-entry-subalist-face-2              ((,class (:foreground ,boat-yellow-1))))
 `(imenu-list-entry-subalist-face-3              ((,class (:foreground ,dragon-white))))

 ;; which-key
 `(which-key-key-face                            ((,class (:inherit font-lock-variable-name-face))))
 `(which-func                                    ((,class (:inherit font-lock-function-name-face :weight bold))))
 `(which-key-group-description-face              ((,class (:foreground ,wave-red))))
 `(which-key-command-description-face            ((,class (:foreground ,crystal-blue))))
 `(which-key-local-map-description-face          ((,class (:foreground ,dragon-yellow))))
 `(which-key-posframe                            ((,class (:background ,wave-blue-1))))
 `(which-key-posframe-border                     ((,class (:background ,wave-blue-1))))

 ;; swiper
 `(swiper-line-face                              ((,class (:foreground ,dragon-yellow))))
 `(swiper-background-match-face-1                ((,class (:background ,surimi-orange :foreground ,dragon-black-0))))
 `(swiper-background-match-face-2                ((,class (:background ,crystal-blue :foreground ,dragon-black-0))))
 `(swiper-background-match-face-3                ((,class (:background ,boat-yellow-1 :foreground ,dragon-black-0))))
 `(swiper-background-match-face-4                ((,class (:background ,peach-red :foreground ,dragon-black-0))))
 `(swiper-match-face-1                           ((,class (:inherit swiper-background-match-face-1))))
 `(swiper-match-face-2                           ((,class (:inherit swiper-background-match-face-2))))
 `(swiper-match-face-3                           ((,class (:inherit swiper-background-match-face-3))))
 `(swiper-match-face-4                           ((,class (:inherit swiper-background-match-face-4))))

 `(counsel-outline-default                       ((,class (:foreground ,dragon-yellow))))
 `(info-header-xref                              ((,class (:foreground ,dragon-yellow))))
 `(xref-file-header                              ((,class (:foreground ,dragon-yellow))))
 `(xref-match                                    ((,class (:foreground ,dragon-yellow))))

 ;; rainbow delimiters
 `(rainbow-delimiters-mismatched-face            ((,class (:foreground ,peach-red))))
 `(rainbow-delimiters-unmatched-face             ((,class (:foreground ,wave-aqua-2))))
 `(rainbow-delimiters-base-error-face            ((,class (:foreground ,peach-red))))
 `(rainbow-delimiters-base-face                  ((,class (:foreground ,dragon-black-4))))

 `(rainbow-delimiters-depth-1-face               ((,class (:foreground ,spring-violet-2))))
 `(rainbow-delimiters-depth-2-face               ((,class (:foreground ,dragon-blue))))
 `(rainbow-delimiters-depth-3-face               ((,class (:foreground ,spring-violet-1))))
 `(rainbow-delimiters-depth-4-face               ((,class (:foreground ,spring-green))))
 `(rainbow-delimiters-depth-5-face               ((,class (:foreground ,wave-aqua-2))))
 `(rainbow-delimiters-depth-6-face               ((,class (:foreground ,dragon-yellow))))
 `(rainbow-delimiters-depth-7-face               ((,class (:foreground ,wave-red))))
 `(rainbow-delimiters-depth-8-face               ((,class (:foreground ,light-blue))))
 `(rainbow-delimiters-depth-9-face               ((,class (:foreground ,spring-violet-2))))

 ;; show-paren
 `(show-paren-match                              ((,class (:background ,wave-aqua-1 :foreground ,dragon-black-0 :weight bold :underline ,(when kanagawa-theme-underline-parens t)))))
 `(show-paren-match-expression                   ((,class (:background ,wave-aqua-1 :foreground ,dragon-black-0 :weight bold))))
 `(show-paren-mismatch                           ((,class (:background ,peach-red :foreground ,old-white :underline ,(when kanagawa-theme-underline-parens t)))))
 `(tooltip                                       ((,class (:foreground ,dragon-black-0 :background ,dragon-yellow :weight bold))))

 ;; company-box
 `(company-tooltip                               ((,class (:background ,dragon-black-2))))
 `(company-tooltip-common                        ((,class (:foreground ,dragon-orange))))
 `(company-tooltip-quick-access                  ((,class (:foreground ,dragon-violet))))
 `(company-tooltip-scrollbar-thumb               ((,class (:background ,dragon-red))))
 `(company-tooltip-scrollbar-track               ((,class (:background ,dragon-black-2))))
 `(company-tooltip-search                        ((,class (:background ,dragon-yellow :foreground ,dragon-black-0 :distant-foreground ,dragon-white))))
 `(company-tooltip-selection                     ((,class (:background ,peach-red :foreground ,winter-red :weight bold))))
 `(company-tooltip-mouse                         ((,class (:background ,dragon-black-2 :foreground ,dragon-black-0 :distant-foreground ,dragon-white))))
 `(company-tooltip-annotation                    ((,class (:foreground ,peach-red :distant-foreground ,dragon-black-1))))
 `(company-scrollbar-bg                          ((,class (:inherit tooltip))))
 `(company-scrollbar-fg                          ((,class (:background ,peach-red))))
 `(company-preview                               ((,class (:foreground ,dragon-yellow))))
 `(company-preview-common                        ((,class (:foreground ,peach-red :weight bold))))
 `(company-preview-search                        ((,class (:inherit company-tooltip-search))))
 `(company-template-field                        ((,class (:inherit match))))

 ;; flycheck
 `(flycheck-posframe-background-face             ((,class (:background ,dragon-black-0))))
 `(flycheck-posframe-face                        ((,class (:background ,dragon-black-0))))
 `(flycheck-posframe-info-face                   ((,class (:background ,dragon-black-0 :foreground ,autumn-green))))
 `(flycheck-posframe-warning-face                ((,class (:background ,dragon-black-0 :foreground ,light-blue))))
 `(flycheck-posframe-error-face                  ((,class (:background ,dragon-black-0 :foreground ,samurai-red))))
 `(flycheck-fringe-warning                       ((,class (:foreground ,light-blue))))
 `(flycheck-fringe-error                         ((,class (:foreground ,samurai-red))))
 `(flycheck-fringe-info                          ((,class (:foreground ,autumn-green))))
 `(flycheck-error-list-warning                   ((,class (:foreground ,dragon-orange-2 :weight bold))))
 `(flycheck-error-list-error                     ((,class (:foreground ,samurai-red :weight bold))))
 `(flycheck-error-list-info                      ((,class (:foreground ,wave-aqua-1 :weight bold))))
 `(flycheck-inline-error                         ((,class (:foreground ,samurai-red :background ,winter-red :slant italic :weight bold :height 138))))
 `(flycheck-inline-info                          ((,class (:foreground ,light-blue :background ,winter-blue :slant italic  :weight bold :height 138))))
 `(flycheck-inline-warning                       ((,class (:foreground ,winter-yellow :background ,dragon-yellow :slant italic :weight bold :height 138))))

 ;; indent dots
 `(highlight-indent-guides-character-face        ((,class (:foreground ,dragon-black-3))))
 `(highlight-indent-guides-stack-character-face  ((,class (:foreground ,dragon-black-3))))
 `(highlight-indent-guides-stack-odd-face        ((,class (:foreground ,dragon-black-3))))
 `(highlight-indent-guides-stack-even-face       ((,class (:foreground ,dragon-black-4))))
 `(highlight-indent-guides-stack-character-face  ((,class (:foreground ,dragon-black-3))))
 `(highlight-indent-guides-even-face             ((,class (:foreground ,dragon-black-2))))
 `(highlight-indent-guides-odd-face              ((,class (:foreground ,dragon-black-4))))


 `(highlight-operators-face                      ((,class (:foreground ,dragon-yellow))))
 `(highlight-quoted-symbol                       ((,class (:foreground ,dragon-green))))
 `(highlight-numbers-face                        ((,class (:foreground ,dragon-pink))))
 `(highlight-symbol-face                         ((,class (:background ,wave-blue-1 :foreground ,dragon-blue))))

 ;; ivy
 `(ivy-current-match                             ((,class (:background ,crystal-blue :foreground ,dragon-black-0 :weight bold))))
 `(ivy-action                                    ((,class (:background unspecified :foreground ,dragon-white))))
 `(ivy-grep-line-number                          ((,class (:background unspecified :foreground ,dragon-green))))
 `(ivy-minibuffer-match-face-1                   ((,class (:background unspecified :foreground ,dragon-red))))
 `(ivy-minibuffer-match-face-2                   ((,class (:background unspecified :foreground ,dragon-green))))
 `(ivy-minibuffer-match-highlight                ((,class (:foreground ,dragon-blue))))
 `(ivy-grep-info                                 ((,class (:foreground ,dragon-blue))))
 `(ivy-grep-line-number                          ((,class (:foreground ,dragon-violet))))
 `(ivy-confirm-face                              ((,class (:foreground ,dragon-aqua))))

 ;; posframe's
 `(ivy-posframe                                  ((,class (:background ,dragon-black-2))))
 `(ivy-posframe-border                           ((,class (:background ,dragon-black-3))))

 ;;treemacs
 `(treemacs-directory-collapsed-face             ((,class (:foreground ,dragon-white))))
 `(treemacs-directory-face                       ((,class (:foreground ,dragon-white))))

 `(treemacs-file-face                            ((,class (:foreground ,dragon-white))))

 `(treemacs-git-added-face                       ((,class (:foreground ,surimi-orange))))
 `(treemacs-git-renamed-face                     ((,class (:foreground ,dragon-white))))
 `(treemacs-git-ignored-face                     ((,class (:foreground ,dragon-black-4))))
 `(treemacs-git-unmodified-face                  ((,class (:foreground ,dragon-white))))
 `(treemacs-git-renamed-face                     ((,class (:foreground ,dragon-white))))
 `(treemacs-git-modified-face                    ((,class (:foreground ,spring-green))))

 ;;lsp
 `(lsp-ui-doc-background                         ((,class (:background ,dragon-black-0 :foreground ,peach-red))))
 `(lsp-ui-doc-header                             ((,class (:background ,dragon-black-0 :foreground ,peach-red))))
 `(lsp-ui-doc-border                             ((,class (:background unspecified :foreground unspecified))))
 `(lsp-ui-peek-filename                          ((,class (:foreground ,light-blue))))
 `(lsp-ui-sideline-code-action                   ((,class (:foreground ,dragon-yellow))))
 `(lsp-ui-sideline-current-symbol                ((,class (:foreground ,spring-blue))))
 `(lsp-ui-sideline-symbol                        ((,class (:foreground ,dragon-blue))))

 `(lsp-headerline-breadcrumb-path-error-face     ((,class (:underline (:color ,spring-green :style wave) :foreground ,dragon-black-4 :background ,dragon-black-0))))
 `(lsp-headerline-breadcrumb-path-face           ((,class (:background ,dragon-black-0))))
 `(lsp-headerline-breadcrumb-path-hint-face      ((,class (:background ,dragon-black-0))))
 `(lsp-headerline-breadcrumb-path-info-face      ((,class (:background ,dragon-black-0))))
 `(lsp-headerline-breadcrumb-separator-face      ((,class (:background ,dragon-black-0))))
 `(lsp-headerline-breadcrumb-symbols-face        ((,class (:background ,dragon-black-0))))
 `(lsp-headerline-breadcrumb-project-prefix-face ((,class (:background ,dragon-black-0))))
 `(lsp-headerline-breadcrumb-symbols-error-face  ((,class (:foreground ,peach-red))))

 ;; Eglot
 `(eglot-diagnostic-tag-unnecessary-face         ((,class (:background ,dragon-black-0 :foreground ,fuji-gray))))


 ;; dashboard
 `(dashboard-heading                             ((,class (:foreground ,spring-violet-2 :weight bold))))
 `(dashboard-items-face                          ((,class (:bold unspecified :foreground ,dragon-white))))
 `(dashboard-banner-logo-title                   ((,class (:weight bold :height 200))))
 `(dashboard-no-items-face                       ((,class (:foreground ,dragon-black-4))))

 ;; all-the-icons
 `(all-the-icons-dgreen                          ((,class (:foreground ,wave-aqua-2))))
 `(all-the-icons-green                           ((,class (:foreground ,wave-aqua-2))))
 `(all-the-icons-dpurple                         ((,class (:foreground ,spring-violet-2))))
 `(all-the-icons-purple                          ((,class (:foreground ,spring-violet-2))))

 ;; evil
 `(evil-ex-lazy-highlight                        ((,class (:foreground ,winter-green :background ,autumn-green :weight bold))))
 `(evil-ex-substitute-matches                    ((,class (:foreground ,winter-red :background ,autumn-red :weight bold))))
 `(evil-ex-substitute-replacement                ((,class (:foreground ,surimi-orange :strike-through unspecified :inherit evil-ex-substitute-matches))))
 `(evil-search-highlight-persist-highlight-face  ((,class (:background ,dragon-yellow))))

 ;; term
 `(term                                          ((,class (:background ,dragon-black-0 :foreground ,dragon-white))))
 `(term-color-blue                               ((,class (:background ,crystal-blue :foreground ,crystal-blue))))
 `(term-color-bright-blue                        ((,class (:inherit term-color-blue))))
 `(term-color-green                              ((,class (:background ,wave-aqua-2 :foreground ,wave-aqua-2))))
 `(term-color-bright-green                       ((,class (:inherit term-color-green))))
 `(term-color-black                              ((,class (:background ,dragon-black-0 :foreground ,dragon-white))))
 `(term-color-bright-black                       ((,class (:background ,dragon-black-1 :foreground ,dragon-black-1))))
 `(term-color-white                              ((,class (:background ,dragon-white :foreground ,dragon-white))))
 `(term-color-bright-white                       ((,class (:background ,old-white :foreground ,old-white))))
 `(term-color-red                                ((,class (:background ,peach-red :foreground ,peach-red))))
 `(term-color-bright-red                         ((,class (:background ,spring-green :foreground ,spring-green))))
 `(term-color-yellow                             ((,class (:background ,dragon-yellow :foreground ,dragon-yellow))))
 `(term-color-bright-yellow                      ((,class (:background ,dragon-yellow :foreground ,dragon-yellow))))
 `(term-color-cyan                               ((,class (:background ,spring-blue :foreground ,spring-blue))))
 `(term-color-bright-cyan                        ((,class (:background ,spring-blue :foreground ,spring-blue))))
 `(term-color-magenta                            ((,class (:background ,spring-violet-2 :foreground ,spring-violet-2))))
 `(term-color-bright-magenta                     ((,class (:background ,spring-violet-2 :foreground ,spring-violet-2))))

 ;; popup
 `(popup-face                                    ((,class (:inherit tooltip))))
 `(popup-selection-face                          ((,class (:inherit tooltip))))
 `(popup-tip-face                                ((,class (:inherit tooltip))))

 ;; anzu
 `(anzu-match-1                                  ((,class (:foreground ,wave-aqua-2 :background ,dragon-black-2))))
 `(anzu-match-2                                  ((,class (:foreground ,dragon-yellow :background ,dragon-black-2))))
 `(anzu-match-3                                  ((,class (:foreground ,light-blue :background ,dragon-black-2))))

 `(anzu-mode-line                                ((,class (:foreground ,dragon-black-0 :background ,spring-violet-2))))
 `(anzu-mode-no-match                            ((,class (:foreground ,dragon-white :background ,peach-red))))
 `(anzu-replace-to                               ((,class (:foreground ,spring-blue :background ,winter-blue))))
 `(anzu-replace-highlight                        ((,class (:foreground ,peach-red :background ,winter-red :strike-through t))))

 ;; ace
 `(ace-jump-face-background                      ((,class (:foreground ,wave-blue-2))))
 `(ace-jump-face-foreground                      ((,class (:foreground ,peach-red :background ,dragon-black-0 :weight bold))))

 ;; vertico
 `(vertico-multiline                             ((,class (:background ,dragon-red))))
 `(vertico-group-title                           ((,class (:background ,winter-blue :foreground ,dragon-blue :weight bold))))
 `(vertico-group-separator                       ((,class (:background ,winter-blue :foreground ,dragon-blue :strike-through t))))
 `(vertico-current                               ((,class (:foreground ,dragon-yellow :weight bold :slant italic :background ,wave-blue-1))))

 `(vertico-posframe-border                       ((,class (:background ,dragon-black-3))))
 `(vertico-posframe                              ((,class (:background ,dragon-black-2))))
 `(orderless-match-face-0                        ((,class (:foreground ,dragon-blue :weight bold))))

 `(comint-highlight-prompt                       ((,class (:background ,dragon-violet :foreground ,dragon-black-1))))
 `(completions-annotations                       ((,class (:background unspecified :foreground ,dragon-blue :slant italic))))
 `(marginalia-file-priv-no                       ((,class (:background unspecified))))

  ;; hydra
 `(hydra-face-amaranth                           ((,class (:foreground ,autumn-red))))
 `(hydra-face-blue                               ((,class (:foreground ,spring-blue))))
 `(hydra-face-pink                               ((,class (:foreground ,sakura-pink))))
 `(hydra-face-red                                ((,class (:foreground ,peach-red))))
 `(hydra-face-teal                               ((,class (:foreground ,light-blue))))

 ;; tab-bar
 `(tab-bar                                       ((,class (:background ,dragon-black-0))))
 `(tab-bar-tab                                   ((,class (:background ,dragon-black-1 :foreground ,dragon-white))))
 `(tab-bar-tab-inactive                          ((,class (:background ,dragon-black-0 :foreground ,dragon-black-4 :height 0.8))))

 ;; centaur-tabs
 `(centaur-tabs-active-bar-face                  ((,class (:background ,spring-blue :foreground ,dragon-white))))
 `(centaur-tabs-selected                         ((,class (:background ,dragon-black-1 :foreground ,dragon-white :weight bold))))
 `(centaur-tabs-selected-modified                ((,class (:background ,dragon-black-1 :foreground ,dragon-white))))
 `(centaur-tabs-modified-marker-selected         ((,class (:background ,dragon-black-1 :foreground ,dragon-orange))))
 `(centaur-tabs-close-selected                   ((,class (:inherit centaur-tabs-selected))))
 `(tab-line                                      ((,class (:background ,dragon-black-0))))

 `(centaur-tabs-unselected                       ((,class (:background ,dragon-black-0 :foreground ,dragon-black-4))))
 `(centaur-tabs-default                          ((,class (:background ,dragon-black-0 :foreground ,dragon-black-4))))
 `(centaur-tabs-unselected-modified              ((,class (:background ,dragon-black-0 :foreground ,peach-red))))
 `(centaur-tabs-modified-marker-unselected       ((,class (:background ,dragon-black-0 :foreground ,dragon-black-4))))
 `(centaur-tabs-close-unselected                 ((,class (:background ,dragon-black-0 :foreground ,dragon-black-4))))

 `(centaur-tabs-close-mouse-face                 ((,class (:background unspecified :foreground ,peach-red))))
 `(centaur-tabs-default                          ((,class (:background ,dragon-orange-2 ))))
 `(centaur-tabs-name-mouse-face                  ((,class (:foreground ,spring-blue :weight bold))))

 `(git-gutter:added                              ((,class (:foreground ,autumn-green))))
 `(git-gutter:deleted                            ((,class (:foreground ,wave-red))))
 `(git-gutter:modified                           ((,class (:foreground ,spring-blue))))

 ;; diff-hl
 `(diff-hl-change                               ((,class (:foreground ,spring-blue :background ,winter-blue))))
 `(diff-hl-delete                               ((,class (:foreground ,peach-red :background ,winter-red))))
 `(diff-hl-insert                               ((,class (:foreground ,comet :background ,winter-blue))))

 `(diff-hl-margin-change                         ((,class (:foreground ,spring-blue :background ,winter-blue))))
 `(diff-hl-margin-delete                         ((,class (:foreground ,peach-red :background ,winter-red))))
 `(diff-hl-margin-insert                         ((,class (:foreground ,comet :background ,winter-blue))))



 `(bm-fringe-face                                ((,class (:background ,peach-red :foreground ,dragon-black-3))))
 `(bm-fringe-persistent-face                     ((,class (:background ,peach-red :foreground ,dragon-black-3))))

 `(ansi-color-green                              ((,class (:foreground ,spring-green))))
 `(ansi-color-black                              ((,class (:background ,dragon-black-0))))
 `(ansi-color-cyan                               ((,class (:foreground ,wave-aqua-2))))
 `(ansi-color-magenta                            ((,class (:foreground ,sakura-pink))))
 `(ansi-color-blue                               ((,class (:foreground ,crystal-blue))))
 `(ansi-color-red                                ((,class (:foreground ,peach-red))))
 `(ansi-color-white                              ((,class (:foreground ,dragon-white))))
 `(ansi-color-yellow                             ((,class (:foreground ,dragon-orange))))
 `(ansi-color-bright-white                       ((,class (:foreground ,old-white))))
 `(ansi-color-bright-white                       ((,class (:foreground ,old-white))))

 `(tree-sitter-hl-face:attribute                 ((,class (:foreground ,surimi-orange))))
 `(tree-sitter-hl-face:escape                    ((,class (:foreground ,wave-red))))
 `(tree-sitter-hl-face:constructor               ((,class (:foreground ,wave-red :weight semi-bold))))

 `(tree-sitter-hl-face:constant                  ((,class (:foreground ,surimi-orange))))
 `(tree-sitter-hl-face:constant.builtin          ((,class (:foreground ,dragon-yellow :weight semi-bold))))

 `(tree-sitter-hl-face:embedded                  ((,class (:foreground ,boat-yellow-2))))


 `(tree-sitter-hl-face:function.builtin          ((,class (:foreground ,peach-red :slant italic :background ,winter-red))))
 `(tree-sitter-hl-face:function.call             ((,class (:foreground ,spring-violet-2))))
 `(tree-sitter-hl-face:function.macro            ((,class (:foreground ,samurai-red))))
 `(tree-sitter-hl-face:function.special          ((,class (:foreground ,sakura-pink))))
 `(tree-sitter-hl-face:function.label            ((,class (:foreground ,surimi-orange))))

 `(tree-sitter-hl-face:method                    ((,class (:foreground ,light-blue))))
 `(tree-sitter-hl-face:method.call               ((,class (:foreground ,light-blue))))

 `(tree-sitter-hl-face:property                  ((,class (:foreground ,dragon-yellow))))
 `(tree-sitter-hl-face:property.definition       ((,class (:foreground ,old-white :slant italic))))

 `(tree-sitter-hl-face:tag                       ((,class (:foreground ,peach-red))))

 `(tree-sitter-hl-face:type                      ((,class (:foreground ,wave-aqua-2 :weight semi-bold))))
 `(tree-sitter-hl-face:type.argument             ((,class (:foreground ,surimi-orange))))
 `(tree-sitter-hl-face:type.builtin              ((,class (:foreground ,autumn-red))))
 `(tree-sitter-hl-face:type.parameter            ((,class (:foreground ,surimi-orange))))
 `(tree-sitter-hl-face:type.super                ((,class (:foreground ,samurai-red :weight bold))))

 `(tree-sitter-hl-face:variable                  ((,class (:foreground ,spring-blue :slant italic))))
 `(tree-sitter-hl-face:variable.builtin          ((,class (:foreground ,wave-red))))
 `(tree-sitter-hl-face:variable.parameter        ((,class (:foreground ,spring-violet-2 :slant italic))))
 `(tree-sitter-hl-face:variable.special          ((,class (:foreground ,surimi-orange))))
 `(tree-sitter-hl-face:variable.synthesized      ((,class (:foreground ,light-blue))))

 `(tree-sitter-hl-face:number                    ((,class (:foreground ,sakura-pink))))
 `(tree-sitter-hl-face:operator                  ((,class (:foreground ,sakura-pink :weight bold))))

 `(tree-sitter-hl-face:punctuation               ((,class (:foreground ,light-blue))))
 `(tree-sitter-hl-face:punctuation.bracket       ((,class (:foreground ,spring-violet-2 :slant italic))))
 `(tree-sitter-hl-face:punctuation.delimiter     ((,class (:foreground ,spring-violet-2 :slant italic))))
 `(tree-sitter-hl-face:punctuation.special       ((,class (:foreground ,peach-red))))

 `(tree-sitter-hl-face:case-pattern              ((,class (:foreground ,wave-red))))
 `(tree-sitter-hl-face:variable.synthesized      ((,class (:foreground ,wave-red))))
 `(tree-sitter-hl-face:keyword.compiler          ((,class (:foreground ,peach-red :slant italic))))

 `(focus-unfocused                               ((,class (:foreground ,dragon-black-4)))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kanagawa)
;;; kanagawa-theme.el ends here



