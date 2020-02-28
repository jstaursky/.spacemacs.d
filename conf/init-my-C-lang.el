;;; init-my-C-lang.el --- C Language editor settings -*- lexical-binding: t -*-
;;; Commentary: File configures the following;
;;; Code:

;; COULD NOT GET THIS package TO WORK WITH use-package.
;; (require 'flex)
;; (add-to-list 'auto-mode-alist '("\\.l\\'\\|\\.lpp\\'\\|\\.lex\\'" . flex-mode))
;; (autoload 'flex-mode "flex")

(use-package bison-mode
  :mode ("\\.y\\'\\|\\.ypp\\'\\|\\.l\\'\\|\\.lpp\\'\\|\\.lex\\'")
  )

;; Works well with C, Does NOT work with C++
;; (use-package ctypes
;;   :load-path "~/.spacemacs.d/local-packages/ctypes"
;;   :config
;;   (ctypes-auto-parse-mode 1)
;;   :hook ((c-mode . (lambda () (require 'ctypes))))
;;   )

(use-package ccls
  :after projectile
  :config
  (setq ccls-args nil)
  (setq ccls-executable (concat dotspacemacs-directory "ccls/Release/ccls"))
  (setq projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package modern-cpp-font-lock
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  )
;; ---------------------------------------------------------------------------

(use-package which-function
  :init
  (defun staur|toggle-headerline ()
    (interactive)
    (if (eq header-line-format nil)
        (setq header-line-format '((which-function-mode
                                    ("" which-func-format " "))))
      ;; else
      (setq header-line-format nil)))

  (mapc (lambda(x) #'(add-to-list 'which-func-modes x))
        (list 'cc-mode 'c-mode 'c++-mode))

  (setq header-line-format
        '((which-function-mode ("" which-func-format " "))))

  ;; Not currently working
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))

  (setq which-func-unknown "n/a")

  (add-hook 'c-mode-common-hook (lambda () (which-function-mode)))

  :hook (c-mode c++-mode)

  )


(use-package cc-mode ;; ------------------------------------------------------
  :init
  (add-hook 'c-mode-hook '(lambda () (c-toggle-comment-style -1))) ; prefer // style comments
  (setq c-default-style "linux"
        c-basic-offset tab-width)

  :config
  (c-set-offset 'inline-open '0)

  (defun my|narrow-to-defun ()
    (interactive)
    (clone-indirect-buffer (buffer-name) t)
    (narrow-to-defun)
    )

  :bind ("C-x /" . my|narrow-to-defun)
  ) ;; end of (use-package cc-mode)...........................................
;; ...........................................................................
;; ---------------------------------------------------------------------------
(use-package helm-gtags-mode ;; ----------------------------------------------
  :hook (c-mode c++-mode)
  :init
  ;; MUST put following in :init, putting in :config does not work
  (defun helm-gtags-dwim-other-window ()
    "helm-gtags-dwim in the other window"
    (interactive)
    (let ((helm-gtags--use-otherwin t)
          (split-height-threshold nil)
          (split-width-threshold 140))
      (helm-gtags-dwim)))

  (defun spacemacs/helm-gtags-define-keys-for-mode (mode)
    (spacemacs/set-leader-keys-for-major-mode mode
      ","  'c-toggle-comment-style
      "o." 'staur|toggle-headerline
      "oc" 'helm-gtags-create-tags
      "ou" 'helm-gtags-update-tags
      "od" 'helm-gtags-dwim
      "og" 'helm-gtags-dwim-other-window
      "of" 'helm-gtags-tags-in-this-function
      "or" 'helm-gtags-find-rtag
      "oh" 'helm-xref-show-xrefs-27
      "oF" 'helm-gtags-find-symbol
      "os" 'helm-gtags-select
      "ol" 'helm-gtags-parse-file
      "oS" 'helm-gtags-show-stack
      "op" 'helm-gtags-pop-stack
      "on" 'helm-gtags-next-history
      "oN" 'helm-gtags-previous-history
      "oC" 'helm-gtags-clear-all-stacks
      "oD" 'nasm-disaster
      )
    )
  (spacemacs/helm-gtags-define-keys-for-mode 'c-mode)
  (spacemacs/helm-gtags-define-keys-for-mode 'c++-mode)
  ) ;; end of (use-package helm-gtags-mode)...................................
;; ...........................................................................
;; ---------------------------------------------------------------------------
(use-package lsp-mode ;; -----------------------------------------------------
  :config
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (dolist (my|lsp-facefix '(lsp-face-highlight-textual
                            lsp-face-highlight-read
                            lsp-face-highlight-write))

    (add-hook 'lsp-mode-hook
              (lambda () (set-face-attribute my|lsp-facefix nil
                           :background
                             (color-darken-name (face-background 'default) -15)
                           :foreground
                             'unspecified
                           :distant-foreground 'unspecified :weight 'normal)))
    )
  )
;; end of (use-package lsp-mode)..........................................
;; ...........................................................................
;; ---------------------------------------------------------------------------
(use-package disaster ;; -----------------------------------------------------
  :defer t
  :commands (disaster)
  :config
  (setq disaster-objdump "objdump -r -d -M intel -m i386 -Sl -C --no-show-raw-insn") ; --no-show-raw-insn -j .text 

  (setq disaster-cflags
  "-m32 -march=i386 -masm=intel -fverbose-asm -mpreferred-stack-boundary=2 -fno-pie -no-pie -fno-stack-protector -fno-asynchronous-unwind-tables -fno-exceptions -fno-tree-vectorize -funroll-loops -fno-inline-functions -fno-inline-functions-called-once -fno-inline-small-functions")

  (setq disaster-cxxflags
  "-m32 -march=i386 -masm=intel -fverbose-asm -mpreferred-stack-boundary=2 -fno-pie -no-pie -fno-stack-protector -fno-asynchronous-unwind-tables -fno-exceptions -fno-rtti -fno-tree-vectorize -funroll-loops -fno-inline-functions -fno-inline-functions-called-once -fno-inline-small-functions")


  (defun buffer-content (&optional buffer-or-name)
    (with-current-buffer (if buffer-or-name buffer-or-name (current-buffer))
      (buffer-substring-no-properties (point-min) (point-max)  )))

  (defadvice disaster--shadow-non-assembly-code (before asm-fixup activate)
    "Tweaks the asm output of disaster to align relocation
symbols in objdump output, and converts hex numbers to their
decimal equivalent"
    (with-current-buffer (current-buffer)

      ;; re-indent relocation entries
      (while (search-forward-regexp "^\\s-+\\(.+?R_.+\\)$" nil t)
        (replace-match (match-string 1))
        (back-to-indentation)
        (indent-relative t))
      (goto-char (point-min))
      (while (search-forward-regexp ":[[:space:]]\\(?:R_\\)" nil t)
        (replace-match ":\tR_")
        (evil-backward-WORD-begin)
        (indent-relative t))
      (goto-char (point-min))

      ;; reindent every start of instruction.
      (while (search-forward-regexp "\\(^\\s-+[a-f0-9]+:\\)[[:space:]]+" nil t)
        (replace-match (concat (match-string 1) "\t")))
      (goto-char (point-min))

      ;; Make lower case data size references.
      (while (search-forward-regexp "\\(^.*? PTR \\[\\)" nil t)
        (replace-match (downcase (match-string 0))))
      (goto-char (point-min))

      ;; Add spaces around plus's and minus's.
      (while (search-forward-regexp "\\b\\([+-]\\)\\b" nil t)
        (replace-match (concat " " (match-string 1) " ") ))
      (goto-char (point-min))

      ;; Add space after comma's.
      (while (search-forward-regexp "\\(,\\)" nil t)
        (replace-match (concat (match-string 1) " ") ))
      (goto-char (point-min))

      ;; Convert hex to decimal inside [].
      (while (search-forward-regexp "\\[\\(.*?\\)\\b0x\\([a-f0-9]+?\\)\\]" nil t)
        (replace-match (concat "[" (match-string 1)
                               (number-to-string (string-to-number (match-string 2) 16)) "]")))
      (goto-char (point-min))

      ;; Format filenames "\\.c[cp]?p?$"
      (while (search-forward-regexp "^\\(.*\.c[cp]?p?\\)\\(:[0-9]+\\)" nil t)
        (replace-match (concat "\n" "  " (file-name-nondirectory (match-string 1)) (match-string 2) "\n"))
        (insert-char (char-from-name "EQUALS SIGN") 62))
      (goto-char (point-min))

      ;; Convert hex to decimal in add and sub instructions.
      (while (search-forward-regexp "\\(add\\|sub\\)\\(\s.*?\\)\\b0x\\([a-f0-9]+\\)$" nil t)
        (replace-match (concat (match-string 1) (match-string 2)
                               (number-to-string (string-to-number (match-string 3) 16)))))

      ;; Remove empty lines.
      (flush-lines "^$" (point-min) (point-max))

      ;; Set stuff up again for processing by disaster-mode.
      (let ((old-point (point))
            (text (buffer-content)))
        (erase-buffer)
        (insert text)
        (goto-char old-point))

      )
    )


  :bind (("C-c n" . view-nasm-disasm)
         )
  ) ;; end of (use-package disaster)..........................................
;;     ____       __      __           __   ____                 __  _
;;    / __ \___  / /___ _/ /____  ____/ /  / __/_  ______  _____/ /_(_)___  ____  _____
;;   / /_/ / _ \/ / __ `/ __/ _ \/ __  /  / /_/ / / / __ \/ ___/ __/ / __ \/ __ \/ ___/
;;  / _, _/  __/ / /_/ / /_/  __/ /_/ /  / __/ /_/ / / / / /__/ /_/ / /_/ / / / (__  )
;; /_/ |_|\___/_/\__,_/\__/\___/\__,_/  /_/  \__,_/_/ /_/\___/\__/_/\____/_/ /_/____/
;; Functions relevent for use in disaster-mode
;; "^.+:\\(\t.+?\t\\)"

(defun view-nasm-disasm (&optional file)
  (interactive)
  (require 'disaster)
  (save-buffer)
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
         (makebuf (get-buffer-create "disaster-buffer-compiler"))
         (nasm-buf (get-buffer-create "nasm-buffer-assembly"))
        )
    (if (not (string-match "\\.c[cp]?p?$" file))
        (message "Not C/C++ non-header file")
      (let*
          (
             (cwd (file-name-directory (expand-file-name (buffer-file-name))))
             (proj-root (disaster-find-project-root nil file))
             (make-root (disaster-find-build-root proj-root))
             (rel-file (if proj-root
                           (file-relative-name file proj-root)
                         file))
             (rel-obj (concat (file-name-sans-extension rel-file) ".o"))
             (obj-file (concat make-root rel-obj))
             (cc (if make-root
                     (if (equal cwd make-root)
                         (format "make %s %s" disaster-make-flags (shell-quote-argument rel-obj))
                       (format "make %s -C %s %s"
                               disaster-make-flags make-root
                               rel-obj))
                   (if (string-match "\\.c[cp]p?$" file)
                       (format "%s %s -g -c -o %s %s"
                               disaster-cxx disaster-cxxflags
                               (shell-quote-argument obj-file) (shell-quote-argument file))
                     (format "%s %s -g -c -o %s %s"
                             disaster-cc disaster-cflags
                             (shell-quote-argument obj-file) (shell-quote-argument file))))
                 )
             (nasm-dis-arg
              (concat (format "objdump -d %s | " (shell-quote-argument (concat make-root rel-obj)))
                      "cut -d: -f2 | cut -d$'\t' -f2 | "
                      "perl -ne 'next if /file/; s/\s+//g; print' | xxd -r -p"))

             (ndisasm (format "ndisasm -b 32 <(%s) " nasm-dis-arg))
             (dump (concat ndisasm "| awk '{printf(\"%-10s %-16s %-6s %s \", $1, $2, $3, $4); for(i=5;i<=NF;i++){printf \"%s \", $i}; printf \"\\n\"}'"))
           )

        (if (and (eq 0 (progn
                         (message (format "Running: %s" cc))
                         (shell-command cc makebuf)))
                 (file-exists-p obj-file))
            (when (eq 0 (progn
                          (message "Running: %s" dump)
                          (shell-command dump nasm-buf)))
              (kill-buffer makebuf)
              (with-current-buffer nasm-buf
                (whitespace-cleanup)
                (nasm-mode)
                ;; Add spaces around plus's and minus's.
                (while (search-forward-regexp "\\b\\([+-]\\)\\b" nil t)
                  (replace-match (concat " " (match-string 1) " ") ))
                (goto-char (point-min))

                ;; Add space after comma's.
                (while (search-forward-regexp "\\(,\\)" nil t)
                  (replace-match (concat (match-string 1) " ") ))
                (goto-char (point-min))

                ;; Convert hex to decimal inside [].
                (while (search-forward-regexp "\\[\\(.*?\\)\\b0x\\([a-f0-9]+?\\)\\]" nil t)
                  (replace-match (concat "[" (match-string 1)
                                         (number-to-string (string-to-number (match-string 2) 16)) "]")))
                (goto-char (point-min))

                ;; Convert hex to decimal in add and sub instructions.
                (while (search-forward-regexp "\\(add\\|sub\\)\\(\s.*?\\)\\b0x\\([a-f0-9]+\\)$" nil t)
                  (replace-match (concat (match-string 1) (match-string 2)
                                         (number-to-string (string-to-number (match-string 3) 16)))))
                (goto-char (point-min))

                ;; Make offsets follow the format found in objdump.
                (while (search-forward-regexp "\\(^.+?\\)\\s-" nil t)
                  (replace-match (format "%2x:" (string-to-number (match-string 1) 16)) t))


                )
              )
          (with-current-buffer makebuf
            (save-excursion
              (goto-char 0)
              (insert (concat cc "\n")))
            (compilation-mode)
            (display-buffer makebuf)))
        )
      )
    ))


(defun nasm-disaster (&optional file line)
  "Shows assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a Makefile in this directory? Run `make bufname.o`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.o`.
- Or is this a C file? Run `cc -g -O3 -c -o bufname.o bufname.c`
- Or is this a C++ file? Run `c++ -g -O3 -c -o bufname.o bufname.c`
- If build failed, display errors in compile-mode.
- Run objdump inside a new window while maintaining focus.
- Jump to line matching current line.

If FILE and LINE are not specified, the current editing location
is used."
  (interactive)
  (require 'disaster)
  (save-buffer)
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
         (line (or line (line-number-at-pos)))
         (file-line (format "%s:%d" file line))
         (makebuf (get-buffer-create "disaster-buffer-compiler"))
         (nasmbuf (get-buffer-create "nasm-disaster-buffer-assembly"))
         (ndisaster-objdump "objdump -r -d -M intel -m i386 -Sl -C --show-raw-insn")
         (opcodes-feed)
         )
    (if (not (string-match "\\.c[cp]?p?$" file))
        (message "Not C/C++ non-header file")
      (let* ((cwd (file-name-directory (expand-file-name (buffer-file-name))))
             (proj-root (disaster-find-project-root nil file))
	     (make-root (disaster-find-build-root proj-root))
	     (rel-file (if proj-root
			   (file-relative-name file proj-root)
			 file))
	     (rel-obj (concat (file-name-sans-extension rel-file) ".o"))
	     (obj-file (concat make-root rel-obj))
             (cc (if make-root
                     (if (equal cwd make-root)
                         (format "make %s %s" disaster-make-flags (shell-quote-argument rel-obj))
                       (format "make %s -C %s %s"
                               disaster-make-flags make-root
                               rel-obj))
                   (if (string-match "\\.c[cp]p?$" file)
                       (format "%s %s -g -c -o %s %s"
                               disaster-cxx disaster-cxxflags
                               (shell-quote-argument obj-file) (shell-quote-argument file))
                     (format "%s %s -g -c -o %s %s"
                             disaster-cc disaster-cflags
                             (shell-quote-argument obj-file) (shell-quote-argument file)))))
             (dump (format "%s %s" ndisaster-objdump
			   (shell-quote-argument (concat make-root rel-obj))))
             (line-text (buffer-substring-no-properties
                         (point-at-bol)
                         (point-at-eol))))
        (if (and (eq 0 (progn
                         (message (format "Running: %s" cc))
                         (shell-command cc makebuf)))
                 (file-exists-p obj-file))
            (when (eq 0 (progn
                          (message (format "Running: %s" dump))
                          (shell-command dump nasmbuf)))
              (kill-buffer makebuf)
              (with-current-buffer nasmbuf
                ;; saveplace.el will prevent us from hopping to a line.
                (set (make-local-variable 'save-place) nil)
                (nasm-mode)

                (while (search-forward-regexp "\\(^.+?\\):\t\\([^R].+?\t\\)\\(.+\\)$" nil t)
                  (setq opcodes-feed (concat opcodes-feed (replace-regexp-in-string "[[:space:]]" "" (match-string 2))))
                  (replace-match

                   (concat (match-string 1) ":\t"
                           (shell-command-to-string
                            (concat (format "echo -n %s | xxd -r -p | ndisasm -u -k 0,%s - | perl -ne 'next if /skipping/; print' | awk '{$1=$2=\"\"; print $0}'"
                                            opcodes-feed
                                            (number-to-string (string-to-number (match-string 1) 16))
                                            )
                                    "| awk '{printf(\"  %-6s %s \", $1, $2); for(i=3;i<=NF;i++){printf \"%s \", $i}; printf \"\\n\"}'"
                                    )
                            ;; (concat (format "echo -n %s | tr -d ' ' | xxd -r -p | ndisasm -u - | awk '{$1=$2= \"\"; print $0;}'"
                            ;;                 (match-string 2)) "| awk '{printf(\"  %-6s %s \", $1, $2); for(i=3;i<=NF;i++){printf \"%s \", $i}; printf \"\\n\"}'")
                            )
                           )
                   )
                  )
                (goto-char (point-min))
                (whitespace-cleanup)




                (disaster--shadow-non-assembly-code))
              (let ((oldbuf (current-buffer)))
                (switch-to-buffer-other-window nasmbuf)
                (goto-char 0)
                (if (or (search-forward line-text nil t)
                        (search-forward file-line nil t))
                    (progn
                      (recenter)
                      (overlay-put (make-overlay (point-at-bol)
                                                 (1+ (point-at-eol)))
                                   'face 'region))
                    (message "Couldn't find corresponding assembly line."))
                (switch-to-buffer-other-window oldbuf)))
          (with-current-buffer makebuf
            (save-excursion
              (goto-char 0)
              (insert (concat cc "\n")))
            (compilation-mode)
            (display-buffer makebuf)))))))





;;     _______   ______
;;    / ____/ | / / __ \
;;   / __/ /  |/ / / / /
;;  / /___/ /|  / /_/ /
;; /_____/_/ |_/_____/
;; End of functions related to disaster-mode
;; ...........................................................................
;; ...........................................................................

(use-package nasm-mode
  :init
  (add-hook 'nasm-mode-hook
            '(lambda ()
               (font-lock-add-keywords nil
                 '(
                   ;; R_386_PC32 -- Relocate a reference that uses a 32-bit
                   ;; PC-relative address.
                   ;; Effective address = PC + <instruction encoded addr>
                   ("R_386_PC32" . font-lock-builtin-face)

                   ;; R_386_32 -- Absolute addressing
                   ;; Uses the value encoded in the instruction.
                   ("R_386_32" . font-lock-builtin-face)

                   ("R_386_.+?\\b" . font-lock-builtin-face)

                   )
                 )))
  :config
  (add-hook 'asm-mode-hook 'nasm-mode)
  )


(defvar my-c-fmt-style
 (concat
          "{ "
          ;;"BasedOnStyle: LLVM,"
          "AccessModifierOffset: -4,"
          "AlignAfterOpenBracket: Align,"
          "AlignConsecutiveAssignments: false,"
          "AlignConsecutiveDeclarations: false,"
          "AlignEscapedNewlines: Left,"
          "AlignOperands: true,"
          "AlignTrailingComments: false,"
          "AllowAllParametersOfDeclarationOnNextLine: false,"
          "AllowShortBlocksOnASingleLine: false,"
          "AllowShortCaseLabelsOnASingleLine: false,"
          "AllowShortFunctionsOnASingleLine: Inline,"
          "AllowShortIfStatementsOnASingleLine: false,"
          "AllowShortLoopsOnASingleLine: false,"
          "AlwaysBreakAfterDefinitionReturnType: None,"
          "AlwaysBreakAfterReturnType: None,"
          "AlwaysBreakBeforeMultilineStrings: false,"
          "AlwaysBreakTemplateDeclarations: false,"
          "BinPackArguments: true,"
          "BinPackParameters: true,"
          "BreakBeforeBraces: Custom,"
          "BraceWrapping: "
          "{"
          "  AfterClass: false,"
          "  AfterControlStatement: false,"
          "  AfterEnum: false,"
          "  AfterFunction: true,"
          "  AfterNamespace: true,"
          "  AfterObjCDeclaration: false,"
          "  AfterStruct: false,"
          "  AfterUnion: false,"
          "  AfterExternBlock: false,"
          "  BeforeCatch: false,"
          "  BeforeElse: false,"
          "  IndentBraces: false,"
          "  SplitEmptyFunction: true,"
          "  SplitEmptyRecord: true,"
          "  SplitEmptyNamespace: true"
          " }, "
          "BreakBeforeBinaryOperators: None,"
          "BreakBeforeInheritanceComma: false,"
          "BreakBeforeTernaryOperators: false,"
          "BreakConstructorInitializersBeforeComma: false,"
          "BreakConstructorInitializers: AfterColon,"
          "BreakStringLiterals: false,"
          "ColumnLimit: 80,"
          "CommentPragmas: '^ IWYU pragma:',"
          "CompactNamespaces: false,"
          "ConstructorInitializerAllOnOneLineOrOnePerLine: false,"
          "ConstructorInitializerIndentWidth: 0,"
          "ContinuationIndentWidth: 4,"
          "Cpp11BracedListStyle: false,"
          "DerivePointerAlignment: false,"
          "DisableFormat: false,"
          "ExperimentalAutoDetectBinPacking: false,"
          "FixNamespaceComments: false,"
          "IncludeBlocks: Preserve,"
          "IncludeIsMainRegex: '(Test)?$',"
          "IndentCaseLabels: false,"
          "IndentPPDirectives: None,"
          "IndentWidth: 4,"
          "IndentWrappedFunctionNames: false,"
          "JavaScriptQuotes: Leave,"
          "JavaScriptWrapImports: true,"
          "KeepEmptyLinesAtTheStartOfBlocks: false,"
          "MacroBlockBegin: '',"
          "MacroBlockEnd: '',"
          "MaxEmptyLinesToKeep: 1,"
          "NamespaceIndentation: Inner,"
          "PenaltyBreakBeforeFirstCallParameter: 30,"
          "PenaltyBreakComment: 10,"
          "PenaltyBreakFirstLessLess: 0,"
          "PenaltyBreakString: 10,"
          "PenaltyExcessCharacter: 100,"
          "PenaltyReturnTypeOnItsOwnLine: 60,"
          "PointerAlignment: Left,"
          "ReflowComments: false,"
          "SortIncludes: false,"
          "SortUsingDeclarations: false,"
          "SpaceAfterCStyleCast: false,"
          "SpaceAfterTemplateKeyword: true,"
          "SpaceBeforeAssignmentOperators: true,"
          "SpaceBeforeParens: Always,"
          "SpaceInEmptyParentheses: false,"
          "SpacesBeforeTrailingComments: 1,"
          "SpacesInAngles: false,"
          "SpacesInContainerLiterals: false,"
          "SpacesInCStyleCastParentheses: false,"
          "SpacesInParentheses: false,"
          "SpacesInSquareBrackets: false,"
          "Standard: Cpp11,"
          "TabWidth: 4,"
          "UseTab: Never"            ; alt Always
          "}"
          ) )

;; Lifted from https://eklitzke.org/smarter-emacs-clang-format
(use-package clang-format

  :config
  (defun clang-format-buffer-smart ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
      (clang-format-buffer)))

  (defun clang-format-buffer-smart-on-save ()
    "Add auto-save hook for clang-format-buffer-smart."
    (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

  (spacemacs/add-to-hooks 'clang-format-buffer-smart-on-save '(c-mode-hook c++-mode-hook))


  (defun my|clang-format-region (s e)
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (point) (point))))
    (clang-format-region
     s e my-c-fmt-style))

  (setq clang-format-style 'my-c-fmt-style)

  ) ; end use-package clang-format

(use-package clang-format
  :if (featurep 'lsp-mode)
  :config (advice-add 'lsp-format-buffer :override #'(lambda ()(my|clang-format-region (point-min) (point-max))))
  ) ; end use-package clang-format

;; ...........................................................................
;; ...........................................................................
;; ...........................................................................
(provide 'init-my-C-lang)
