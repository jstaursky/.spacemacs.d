;;; init-personal.el --- Language independent editor settings -*- lexical-binding: t -*-
;;; Commentary: File configures the following;
;;;   - indentation
;;;   - evil-escape-delay
;;;   - SPC o prefix leader keys.
;;;   - Shell paths
;;; Code:


;; Replaces exec-path-from-shell
;; May need to restart before it works. Check this by starting emacs from
;; shell, if behavior is different than when launched from menu, you just need
;; to restart to fix.
(setenv "PATH" (mapconcat 'identity (split-string-and-unquote
                 (getenv-internal "PATH" initial-environment) ":") ":"))


(setq-default evil-escape-delay 0.3)



(setq-default tab-width 4
              indent-tabs-mode nil      ; use space to indent by default
              truncate-lines   t)       ; Don't fold long lines


;; Favorite Keybinding
(global-set-key (kbd "M-i") 'indent-relative)


(use-package lsp-mode
  :config
  (setq lsp-eldoc-hook nil))


(spacemacs/set-leader-keys
  "op" 'open-personal-init-settings
  "oo" 'open-org-tasks
  "os" 'open-org-school-tasks
  "oh" 'which-key-show-major-mode
  )

;; ...........................................................................
;;             _                    __                  _   _
;;   /\  /\___| |_ __   ___ _ __   / _|_   _ _ __   ___| |_(_) ___  _ __  ___
;;  / /_/ / _ \ | '_ \ / _ \ '__| | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; / __  /  __/ | |_) |  __/ |    |  _| |_| | | | | (__| |_| | (_) | | | \__ \
;; \/ /_/ \___|_| .__/ \___|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
;;              |_|
;; Helper Functions
;;

(defun open-personal-init-settings () (interactive)
       "Open conf/ directory for selecting personal init files."
       (require 'helm)
       (helm-find-files-1 "~/Dropbox/.spacemacs.d/conf/"))

(defun open-org-tasks () (interactive)
       (find-file "~/Dropbox/Org/tasks.org"))
(defun open-org-school-tasks () (interactive)
       (find-file "~/Dropbox/School/FALL2019/Fall2019.org"))

;; ...........................................................................
;; ...........................................................................
;; ...........................................................................
(provide 'init-personal)
