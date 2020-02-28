;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t
                     spell-checking-enable-auto-dictionary t
                     )
     rust
     graphviz
     (python ;:packages ()
             :variables python-backend 'lsp python-lsp-server 'pyls
             )
     html
     (perl5 :packages (not company-plsense))
     latex
     helm
     ;; ----------------------------------------------------------------
     (org :variables
          org-enable-sticky-header t)
     pdf
     git
     (version-control  :variables
                       version-control-diff-side  'left
                       version-control-diff-tool  'diff-hl)

     (auto-completion :variables
                      auto-completion-enable-help-tooltip       t
                      auto-completion-enable-snippets-in-popup  t
                      auto-completion-enable-sort-by-usage      t
                      auto-completion-return-key-behavior       'complete
                      auto-completion-tab-key-behavior          'complete
                      :disabled-for org)

     emacs-lisp
     markdown
     multiple-cursors
     (shell  :variables
             shell-default-term-shell "/usr/bin/zsh"
             shell-default-height 30
             shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     treemacs
;;     dap ; (d)ebug (a)dapter (p)rotocol
     (c-c++ :variables ; c++-enable-organize-includes-on-save t
            c-c++-backend 'lsp-ccls
            c-c++-adopt-subprojects t
            )
     (lsp :variables
          ; lsp-workspace-folders-remove <- Don't forget about this function!
          ; lsp-remap-xref-keybindings t
          lsp-ui-sideline-enable     nil
          lsp-ui-doc-enable          nil)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     (olivetti :fetcher github
               :repo "rnkn/olivetti")
     (atom-one-dark-theme :fetcher github :repo "jonathanchu/atom-one-dark-theme")
     helm-gtags
     bison-mode
     (modern-cpp-font-lock :fetcher github :repo "ludwigpacifici/modern-cpp-font-lock")
     (arjen-grey-theme :repo "credmp/arjen-grey-theme"
                       :fetcher github)
     (cython-mode :fetcher github :repo "cython/cython"
                  :files ("Tools/*.el"))
     (poporg
      :repo "QBobWatson/poporg"
      :fetcher github)
     (cmake-mode :fetcher git
                 :url "https://gitlab.kitware.com/cmake/cmake.git"
                 :files ("Auxiliary/*.el"))

     (flex :location (recipe :fetcher github :repo "manateelazycat/flex"))


     (org-emms :repo "jagrg/org-emms" :fetcher gitlab)
     (emms
      :url "https://git.savannah.gnu.org/git/emms.git"
      :fetcher git
      :files ("lisp/*.el" "doc/emms.texinfo"))

     (org-ref :fetcher github :repo "jkitchin/org-ref" :files (:defaults "org-ref.org" "org-ref.bib" "citeproc"))
     nasm-mode
     (all-the-icons
      :repo "domtronn/all-the-icons.el"
      :fetcher github
      :files (:defaults "data"))
     (tron-theme :location local)
     (weyland-yutani-theme :location local)

     ;; (irony :fetcher github
     ;;        :repo "Sarcasm/irony-mode"
     ;;        :files ("*.el" "server"))
     ;; (company-irony :fetcher github :repo "Sarcasm/company-irony")
     ;; (flycheck-irony :fetcher github :repo "Sarcasm/flycheck-irony")

     material-theme
     ample-theme
     flatui-theme
     labburn-theme
     hc-zenburn-theme
     tj3-mode
     tangotango-theme
     (doom-palenight-theme :location local)
     (wilmersdorf-theme :location local)
     (doom-themes :repo "hlissner/emacs-doom-themes" :fetcher
                  github :files (:defaults "themes/*.el"))

     org-pdfview
     org-ref
     (px :location
         (recipe :fetcher github :repo "aaptel/preview-latex"))

     ) ; end dotspacemacs-additional-packages

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     persp-mode
     flycheck-pos-tip
     )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         doom-one
                         doom-dark+
                         hc-zenburn
                         doom-spacegrey
                         doom-moonlight
                         doom-palenight
                         spacemacs-light
                         weyland-yutani
                         doom-spacegrey
                         doom-nord
                         doom-tomorrow-night
                         tron
                         base16-material-palenight
                         base16-tomorrow-night
                         doom-one
                         doom-vibrant
                         spacemacs-dark
                         spacemacs-light
                         wilmersdorf
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator nil :separator-scale 1.0)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '(
                               ;"Hack"
                               ;"Liberation Mono"
                               ;"IBM Plex Mono"
                               ;"League Mono"
                               ;"Operator Mono"
                               ;"Dank Mono"
                               ;"Fira Code"
                               ;"Roboto Mono"
                               ;"Droid Sans Mono Slashed"
                               ;"Noto Sans Mono"
                               ;"Monaco"
                               ;"Courier 10 Pitch:"
                               ;"SF Mono"
                               ;"monoOne"
                               ;"Fira Mono"
                               "Gintronic"
                               ;"Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (add-to-list 'custom-theme-load-path "~/.spacemacs.d/themes/")
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setq frame-title-format
        (list '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  (add-to-list 'load-path
    (expand-file-name "conf" dotspacemacs-directory))

  (require 'init-personal)
  (require 'init-my-org)
  (require 'init-my-C-lang)  ;; C-C++ language settings


  (require 'all-the-icons)

  ;; move line that point is on to window top and move point to top as well.
  (global-set-key [prior] (lambda () (interactive) (recenter 0)))
  (global-set-key [next] 'recenter-top-bottom)



  (use-package python
    :init (setq python-indent-offset 4
                python-shell-interpreter "/usr/bin/python3")
    :config
;   (setq-default lsp-pyls-plugins-preload-modules ["numpy", "pandas"])

    ) ; end use-package python

  (use-package python
    :if (featurep 'lsp-mode)
    :config
    ;; Get rid of annoying pop in of docs at bottom.
    (setq lsp-eldoc-hook nil)           ;; doesn't seem to work
    (fmakunbound 'lsp-signature-activate)
    (defun lsp-signature-activate ()
      (message nil)
      )
    ;; retain the C-S-SPC functionality
    :bind (([remap lsp-signature-activate]
            . (lambda () (interactive)
                (setq-local lsp--last-signature nil)
                (setq-local lsp--last-signature-index nil)
                (setq-local lsp--last-signature-buffer nil)
                (add-hook 'lsp-on-idle-hook #'lsp-signature nil t)
                (add-hook 'post-command-hook #'lsp-signature-maybe-stop)
                (lsp-signature-mode t))))
    ) ; end use-package python


  (use-package lsp-mode
    :config
    (require 'lsp-clients)
    (add-hook 'prog-mode-hook 'lsp))

  (add-hook 'prog-mode-hook 'auto-fill-mode)

  (defun bjm/align-whitespace (start end)
    "Align columns by whitespace"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)\\s-" 1 0 t))
  ;; (windmove-default-keybindings)        ; type S-<left>, S-<right>, etc.. to
  ;;                                       ; move to different windows.

  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  ;; Used to hack together a date setter for org mode deadlines
  (defun minibuffer-input-provider (inputs)
    (let ((hook (make-symbol "hook")))
      (fset hook (lambda ()
                   (remove-hook 'post-command-hook hook)
                   (when inputs
                     (when (= 0 (minibuffer-depth))
                       (error "Too many inputs"))
                     (when (cdr inputs)
                       (add-hook 'post-command-hook hook))
                     (insert (pop inputs))
                     (exit-minibuffer))))
      (add-hook 'post-command-hook hook)))


  (defmacro with-minibuffer-input (form &rest inputs)
    (declare (indent 1))
    `(minibuffer-with-setup-hook
         (lambda ()
           (minibuffer-input-provider ',inputs))
       ,form))

  (fringe-mode '(4 . 0))

  (use-package poporg
    :bind* (("C-c /" . poporg-dwim)))




  (use-package pyret
    :load-path "~/.spacemacs.d/local-packages"
    :commands pyret-mode)

  (use-package sleigh
    :load-path "~/.spacemacs.d/local-packages/sleigh-mode"
    :mode ("\\.slaspec\\'\\|\\.sinc\\'" . sleigh-mode)
    :commands sleigh-mode)

  (use-package picat-mode
    :load-path "~/.spacemacs.d/local-packages/picat-mode"
    )

  (use-package tj3-mode
    :mode "\\.tjp\\'")


  (use-package tex
    :init
    (setq TeX-view-program-selection '((output-pdf "Zathura")))
    (setq TeX-view-program-list
          '(("Zathura"
             ("zathura %o"
              (mode-io-correlate
               " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\"")))))
    (setq TeX-source-correlate-mode t)
    (setq TeX-source-correlate-start-server t)
    (setq TeX-source-correlate-method 'synctex)


    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)

    )  ;; end use-package tex


  (use-package pdf-tools
    :config
    ;; open pdfs scaled to fit page
    (setq-default pdf-view-display-size 'fit-page)
    (setq pdf-annot-edit-contents-setup-function
          (lambda (_annot) (org-mode) (px-preview)))
    )  ;; end use-package pdf-tools

  (use-package px          :after (org))
  (use-package org-pdfview :after (org))


  (require 'emms-setup)
  (emms-all)
  (emms-default-players)

  ;; Configure how diff faces look
  (dolist (my|diff-facefix '(diff-hl-change diff-hl-insert diff-hl-delete))
    (add-hook 'prog-mode-hook (lambda ()
                                (set-face-background my|diff-facefix (face-foreground my|diff-facefix)))))



  (require 'helm-imenu)

  (defun my-helm-imenu-transformer (cands)
    (with-helm-current-buffer
      (save-excursion
        (cl-loop for (func-name . mrkr) in cands
                 collect
                 (cons (format "Line %4d: %s"
                               (line-number-at-pos mrkr)
                               (progn (goto-char mrkr)
                                      (buffer-substring mrkr (line-end-position))))
                       (cons func-name mrkr))))))

  (defvar my-helm-imenu-source  (helm-make-source "Imenu" 'helm-imenu-source
                                  :candidate-transformer
                                  'my-helm-imenu-transformer))
  (defun my-helm-imenu ()
    (interactive)
    (let ((imenu-auto-rescan t)
          (str (thing-at-point 'symbol))
          (helm-execute-action-at-once-if-one
           helm-imenu-execute-action-at-once-if-one))
      (helm :sources 'my-helm-imenu-source
            :preselect str
            :buffer "*helm imenu*")))

  ;; Fixes spacemacs homepage for fonts without unicode support.
  (when (member "Source Code Pro" (font-family-list))
    (set-fontset-font t 'unicode "Source Code Pro" nil 'prepend))


  (defun ejmr/toggle-writing-mode ()
    "Toggle a distraction-free environment for writing."
    (interactive)
    (cond ((bound-and-true-p olivetti-mode)
           (olivetti-mode -1)
           (olivetti-toggle-hide-modeline)
           (toggle-frame-fullscreen)
           (menu-bar-mode 1))
          (t
           (olivetti-mode 1)
           (olivetti-toggle-hide-modeline)
           (toggle-frame-fullscreen)
           (menu-bar-mode -1))))

  (add-hook 'olivetti-mode-hook (lambda () (interactive) (setq olivetti-body-width 110)))

  (spacemacs/set-leader-keys "wo" 'ejmr/toggle-writing-mode)


;;;; END dotspacemacs/user-config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'cl-lib)
  (defalias 'letf 'cl-letf)
  (defalias 'incf 'cl-incf)
  (defalias 'do 'cl-do)
  (defalias 'destructuring-bind 'cl-destructuring-bind)
  (defalias 'org-toggle-latex-fragment 'org-latex-preview)
  )  ;;;

;;             _                    __                  _   _
;;   /\  /\___| |_ __   ___ _ __   / _|_   _ _ __   ___| |_(_) ___  _ __  ___
;;  / /_/ / _ \ | '_ \ / _ \ '__| | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; / __  /  __/ | |_) |  __/ |    |  _| |_| | | | | (__| |_| | (_) | | | \__ \
;; \/ /_/ \___|_| .__/ \___|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
;;              |_|
;; Helper Functions
;;
;; ...........................................................................
;; ...........................................................................
;; ...........................................................................
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
;'(org-agenda-files '("~/Dropbox/School/FALL2019/Fall2019.org"))
 '(package-selected-packages
   '(toml-mode racer flycheck-rust bui tree-mode cargo rust-mode graphviz-dot-mode yapfify stickyfunc-enhance pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements lsp-python-ms live-py-mode importmagic epc ctable concurrent deferred helm-pydoc helm-cscope xcscope ggtags cython-mode counsel-gtags counsel swiper company-anaconda blacken anaconda-mode pythonic camcorder web-mode web-beautify tagedit slim-mode scss-mode sass-mode pug-mode prettier-js impatient-mode simple-httpd helm-css-scss haml-mode emmet-mode counsel-css company-web web-completion-data add-node-modules-path yasnippet-snippets xterm-color ws-butler writeroom-mode winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-evil toc-org symon symbol-overlay string-inflection spaceline-all-the-icons smeargle shell-pop restart-emacs rainbow-delimiters px popwin pcre2el password-generator paradox overseer orgit org-projectile org-present org-pomodoro org-pdfview org-mime org-download org-cliplink org-bullets org-brain open-junk-file nasm-mode nameless multi-term move-text mmm-mode markdown-toc magit-svn magit-gitflow macrostep lsp-ui lsp-treemacs lorem-ipsum link-hint indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-rtags helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-lsp helm-gtags helm-gitignore helm-git-grep helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate google-c-style golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy font-lock+ flycheck-rtags flycheck-package flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-themes doom-modeline disaster diminish diff-hl devdocs define-word dap-mode cquery cpp-auto-include counsel-projectile company-statistics company-rtags company-quickhelp company-lsp company-c-headers column-enforce-mode clean-aindent-mode clang-format centered-cursor-mode ccls browse-at-remote auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line ac-ispell))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
