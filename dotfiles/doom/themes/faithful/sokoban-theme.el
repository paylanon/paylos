;;; sokoban-theme.el --- A retro color theme

;;     //   ) )
;;    ((         ___     / ___      ___     / __      ___       __
;;      \\     //   ) ) //\ \     //   ) ) //   ) ) //   ) ) //   ) )
;;        ) ) //   / / //  \ \   //   / / //   / / //   / / //   / /
;; ((___ / / ((___/ / //    \ \ ((___/ / ((___/ / ((___( ( //   / /

;; Author: paylhorse <paylhorsegames@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/paylhorse/sokoban-theme
;; License: GPL-3+
;;; Commentary:

;; Refined version of the palette used on Jonathan Blow's early programming livestreams.
;; A fork of @nikav's naysayer-theme.

;; ------ Variant: FAITHFUL ------

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The sokoban theme requires Emacs 24 or later!"))

(deftheme sokoban "A retro color theme.")

(defgroup sokoban-theme nil
  "Sokoban theme."
  :group 'faces
  :prefix "sokoban-"
  :link '(url-link :tag "GitHub" "http://github.com/paylhorse/sokoban-theme")
  :tag "Sokoban theme")

;; Global colors
(defcustom sokoban-theme-yellow
  "#E4E0BE"
  "Primary colors: yellow."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-orange
  "#D9B58B"
  "Primary colors - orange."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-red
  "#EDB3C8"
  "Primary colors - red."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-magenta
  "#D7AFD4"
  "Primary colors - magenta."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-blue
  "#98BCC2"
  "Primary colors - blue."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-darkblue
  "#006ab5"
  "Primary colors - darkblue."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-green
  "#D0DCB7"
  "Primary colors - green."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-cyan
  "#D2E7E4"
  "Primary colors - cyan."
  :type 'string
  :group `sokoban-theme)

(defcustom sokoban-theme-violet "#C2B7DC"
  "Primary colors - violet."
  :type 'string
  :group `sokoban-theme)

(let ((background "#062626")
      (gutters    "#062626")
      (gutter-fg  "#062626")
      (gutters-active "#062626")
      (builtin      "#ffffff")
      (selection  "#5f5f6e")
      (text       "#d1b897")
      (comments   "#869b85")
      (punctuation "#a1d19b")
      (keywords "#dddddd")
      (variables "#c1d1e3")
      (functions "#dac6ab")
      (methods    "#c1d1e3")
      (strings    "#9bbfb6")
      (constants "#7ad0c6")
      (macros "#a1d19b")
      (numbers "#7ad0c6")
      (white     "#ffffff")
      (error "#c49191")
      (warning "#ffaa00")
      (highlight-line "#072929")
      (line-fg "#444444"))

  (custom-theme-set-faces
   'sokoban

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,keywords))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,sokoban-theme-violet))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,sokoban-theme-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,sokoban-theme-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,sokoban-theme-yellow))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,sokoban-theme-orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,sokoban-theme-red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,sokoban-theme-violet))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,sokoban-theme-blue))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,sokoban-theme-green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,sokoban-theme-yellow))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,sokoban-theme-orange))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,sokoban-theme-red))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box nil))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,background
                                            :background ,keywords
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))
   `(persp-face-lighter-buffer-not-in-persp ((t (:foreground ,background :background ,error))))

    ;; better compatibility with default DOOM mode-line
   `(error ((t (:foreground nil :weight normal))))
   `(doom-modeline-project-dir ((t (:foreground nil :weight bold))))
   
   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,text))))
   `(js2-jsdoc-tag ((t (:foreground ,keywords))))
   `(js2-jsdoc-type ((t (:foreground ,constants))))
   `(js2-jsdoc-value((t (:foreground ,text))))
   `(js2-object-property ((t (:foreground ,text))))
   `(js2-external-variable ((t (:foreground ,constants))))
   `(js2-error ((t (:foreground ,error))))
   `(js2-warning ((t (:foreground ,warning))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))

   ;; diredfl
   `(diredfl-dir-heading ((t (:foreground ,text :background ,background))))
   `(diredfl-file-name ((t (:foreground ,sokoban-theme-blue))))
   `(diredfl-file-suffix((t (:foreground ,sokoban-theme-blue))))
   `(diredfl-number ((t (:foreground ,keywords))))
   `(diredfl-date-time ((t (:foreground ,comments))))

   ;; avy
   `(avy-lead-face ((t (:background ,sokoban-theme-darkblue, :foreground ,keywords))))

   ;; vterm
   `(vterm-color-red ((t (:foreground ,sokoban-theme-red :background ,sokoban-theme-red))))
   `(vterm-color-blue ((t (:foreground ,sokoban-theme-blue :background ,sokoban-theme-blue))))
   `(vterm-color-cyan ((t (:foreground ,sokoban-theme-cyan :background ,sokoban-theme-cyan))))
   `(vterm-color-black ((t (:foreground ,background :background ,background))))
   `(vterm-color-green ((t (:foreground ,sokoban-theme-green :background ,sokoban-theme-green))))
   `(vterm-color-white ((t (:foreground ,keywords :background ,keywords))))
   `(vterm-color-yellow ((t (:foreground ,sokoban-theme-yellow :background ,sokoban-theme-yellow))))
   `(vterm-color-magenta ((t (:foreground ,sokoban-theme-magenta :background ,sokoban-theme-magenta)))))


  (custom-theme-set-variables
    'sokoban
    '(linum-format " %5i ")))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'sokoban)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'sokoban-theme)

;;; sokoban-theme.el ends here
