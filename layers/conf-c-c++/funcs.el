;;; funcs.el --- Additional C/C++ Layer for Spacemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my-clang-format-region (s e)
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (point) (point))))
    (clang-format-region
     s e (concat
          "{"
          "BasedOnStyle: LLVM,"
          "IndentWidth: 4,"
          "AccessModifierOffset: -4,"
          "BraceWrapping: {"
          "  AfterClass: false,"
          "  AfterControlStatement: false"
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
          " },"
          "Standard: Cpp11"
          "}"
          )))
