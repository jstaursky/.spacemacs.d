;;; packages.el --- C/C++ Layer for Spacemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst conf-c-c++-packages
  '(
    (irony
     :location (recipe :fetcher github :repo "Sarcasm/irony-mode"))

    (company-irony :toggle (configuration-layer/package-usedp 'company)
     :location (recipe :fetcher github
                       :repo "Sarcasm/company-irony"))

    (company-irony-c-headers :toggle
                             (configuration-layer/package-usedp 'company)
     :location (recipe :fetcher github
                       :repo "hotpxl/company-irony-c-headers"))

    (flycheck-irony
     :location (recipe :fetcher github
                       :repo "Sarcasm/flycheck-irony"))

    (clang-format
     :location (recipe :fetcher github
                       :repo "emacsorphanage/clang-format"))
    )
  ) ;; conf-c-c++-packages

;; init functions
;;
(defun conf-c-c++/init-clang-format ()
  (use-package clang-format))

(defun conf-c-c++/init-irony ()
  (use-package irony
    :hook  ((c++-mode  . irony-mode)
            (c-mode    . irony-mode)
            (objc-mode . irony-mode))
    :config (irony-cdb-autosetup-compile-options)
    ))

(defun conf-c-c++/init-company-irony ()
  (use-package company-irony :after irony
    :init    (add-to-list 'company-backends
                          'company-irony)
    :hook    irony-mode-hook
    :config  (add-hook 'irony-mode-hook
                       'company-irony-setup-begin-commands)
    ))

(defun conf-c-c++/init-company-irony-c-headers ()
  (use-package company-irony-c-headers :after (irony company-irony)
    :init (add-to-list 'company-backends
                       'company-irony-c-headers)
    :hook irony-mode-hook
    ))

(defun conf-c-c++/init-flycheck-irony ()
  (use-package flycheck-irony :after irony))

;; Post-init functions
;;
(defun conf-c-c++/post-init-flycheck-irony ()
  (use-package flycheck-irony :after irony
    :init   (add-hook  'flycheck-mode-hook
                      #'flycheck-irony-setup)
    :config (setq flycheck-clang-language-standard "c++11")
    ))
