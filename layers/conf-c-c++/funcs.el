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
          "PointerAlignment: Right,"
          "ReflowComments: false,"
          "SortIncludes: false,"
          "SortUsingDeclarations: false,"
          "SpaceAfterCStyleCast: false,"
          "SpaceAfterTemplateKeyword: true,"
          "SpaceBeforeAssignmentOperators: true,"
          "SpaceBeforeParens: ControlStatements,"
          "SpaceInEmptyParentheses: false,"
          "SpacesBeforeTrailingComments: 1,"
          "SpacesInAngles: false,"
          "SpacesInContainerLiterals: false,"
          "SpacesInCStyleCastParentheses: false,"
          "SpacesInParentheses: false,"
          "SpacesInSquareBrackets: false,"
          "Standard: Cpp11,"
          "TabWidth: 4,"
          "UseTab: Always"
          "}"
          )))

(defun spacemacs/lsp-define-key (keymap key def &rest bindings)
  "Define multiple key bindings with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun spacemacs/lsp-bind-keys ()
  "Define key bindings for the lsp minor mode."
  (spacemacs//lsp-bind-peek-navigation-functions "g")
)

(defun spacemacs//lsp-bind-peek-navigation-functions (prefix-char)
  (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    (concat prefix-char "i") #'lsp-ui-peek-find-implementation
    (concat prefix-char "d") #'lsp-ui-peek-find-definitions
    (concat prefix-char "r") #'lsp-ui-peek-find-references
    (concat prefix-char "p") #'lsp-ui-peek-jump-backward
    (concat prefix-char "n") #'lsp-ui-peek-jump-forward)
  )
