;; Tron Color Theme for Emacs.
;;
;; MIT License Copyright (c) 2012 Ivan Marcin <ivan at ivanmarcin dot com>
;;
;; All patches welcome

;; --------------
;; This porting makes tron no longer rely on color-theme package,
;; since Emacs has it's theme mechanism from Emacs 24.

;; How to use:
;; copy the theme file to your themes folder or create one in your home directory.
;; edit init.el and add this 2 lines:
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme `tron t)
;;
;; or
;; load it manually by pressing
;; M-x load-theme, then choose tron, it should work
;; Or, simple use (load-theme 'tron t) to enable the theme from start.

;;; tron-theme

;;; Code
(deftheme tron
  "Based off Tron Legacy")



(custom-theme-set-faces
 `tron
 `(default ((t (:background "#03060d" :foreground "#c0e3f2")))) ; #a5c4de #bed5de #bed5f1
 `(bold ((t (:bold t))))
 `(bold-italic ((t (:bold t))))
 `(border-glyph ((t (nil))))
 `(fringe ((t (:background "#a4c2cc"))))
 `(mode-line ((t (:foreground "#072d40" :background "#99bac7"))))
 `(region ((t (:background "#356a9c"))))
 `(hl-line ((t (:background "#2c333d")))) ;#414c5b
 `(font-lock-builtin-face ((t (:foreground "#cbd8f9"))))
 `(font-lock-preprocessor-face ((t (:foreground "#c0e3f2"))))
 `(font-lock-comment-face ((t (:foreground "#575b5b"))))
 `(font-lock-function-name-face ((t (:foreground "#c0e3f2"))))
 `(font-lock-doc-face ((t (:foreground "#eff9cb"))))
 `(font-lock-keyword-face ((t (:foreground "#c0e3f2" :bold t))))
 `(font-lock-string-face ((t (:foreground "#f6fce2"))))
 `(font-lock-type-face ((t (:foreground "#87a0be"))))
 `(font-lock-constant-face ((t (:foreground "#eeedec"))))
 `(font-lock-variable-name-face ((t (:foreground "#c0e3f2"))))
 `(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
 `(font-lock-warning-face ((t (:foreground "red" :bold t))))
)

(provide-theme 'tron)

;;eof
