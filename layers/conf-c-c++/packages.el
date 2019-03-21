;;; packages.el --- C/C++ Layer for Spacemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst conf-c-c++-packages
  '(
    company
    irony
    (company-irony           :requires company irony)
    (company-irony-c-headers :requires company irony)
    (flycheck-irony          :requires irony)
    )
  ) ;; conf-c-c++-packages

;; init functions
;;

(defun conf-c-c++/post-init-company ()
  (spacemacs|add-company-backends
    :backends (company-irony company-irony-c-headers) :modes c-mode-common)
  )

(defun conf-c-c++/init-irony ()
  (use-package irony
    :hook  ((c++-mode  . irony-mode)
            (c-mode    . irony-mode)
            (objc-mode . irony-mode))
    :config (irony-cdb-autosetup-compile-options)
    (spacemacs/set-leader-keys-for-major-mode
      'c-mode "t" 'irony-get-type)
    (spacemacs/set-leader-keys-for-major-mode
      'c-mode ";" 'c-toggle-comment-style)
    ))

(defun conf-c-c++/init-company-irony ()
  (use-package company-irony :after irony
    :config (company-irony-setup-begin-commands)
    ))

(defun conf-c-c++/init-company-irony-c-headers ()
  (use-package company-irony-c-headers :after (irony company-irony)
    ))

(defun conf-c-c++/init-flycheck-irony ()
    (use-package flycheck-irony :after irony
      :init   (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
      :config (setq flycheck-clang-language-standard "c++11")
      ))


