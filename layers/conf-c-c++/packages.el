;;; packages.el --- C/C++ Layer for Spacemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst conf-c-c++-packages
  '(
    clang-format
    semantic
    company
    irony
    lsp-mode
    lsp-ui
    company-lsp
    (company-irony           :requires company irony)
    (company-irony-c-headers :requires company irony)
    (flycheck-irony          :requires irony)
    )
  ) ;; conf-c-c++-packages

;; init functions
;;

(defun conf-c-c++/post-init-company ()
  (spacemacs|add-company-backends
    :backends (company-irony company-irony-c-headers company-clang)
    :modes c-mode-common)
  )

(defun conf-c-c++/init-lsp-mode ()
  (use-package lsp-mode
    :commands lsp
    :init
    (setq lsp-prefer-flymake nil)
    (add-hook 'c++-mode-hook #'lsp)
    (add-hook 'c-mode-hook #'lsp)
    )
  )

(defun conf-c-c++/init-lsp-ui ()
  (use-package lsp-ui :commands lsp-ui-mode
    :init
    (setq lsp-ui-doc-enable t
          lsp-ui-peek-enable t
          lsp-ui-sideline-enable nil
          lsp-ui-imenu-enable nil
          lsp-ui-flycheck-enable t)
    )
  )

(defun conf-c-c++/init-company-lsp ()
  (use-package company-lsp :commands company-lsp
    :config
    (push 'company-lsp company-backends)
    )
  )

(defun conf-c-c++/post-init-semantic ()
  (spacemacs/add-to-hooks 'semantic-mode '(c-mode-hook c++-mode-hook))
  )

(defun conf-c-c++/init-clang-format ()
  (use-package clang-format
    :config
    (defun clang-format-buffer-smart ()
      "Reformat buffer if .clang-format exists in the projectile root."
      (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
        (clang-format-buffer)))

    (defun clang-format-buffer-smart-on-save ()
      "Add auto-save hook for clang-format-buffer-smart."
      (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

    (spacemacs/add-to-hooks 'clang-format-buffer-smart-on-save
                            '(c-mode-hook c++-mode-hook)))
  )

(defun conf-c-c++/init-semantic ()
  (use-package semantic
    :init
    (add-hook
     'c-mode-common-hook (lambda () (add-to-list
                                     'semantic-default-submodes
                                     'global-semantic-stickyfunc-mode)
                           (semantic-mode 1)))
    )
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
      'c-mode "," 'c-toggle-comment-style)
    (setq irony-additional-clang-options '("-std=c++11"))
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


