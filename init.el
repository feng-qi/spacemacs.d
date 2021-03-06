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
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     (ivy :variables
          ivy-count-format "(%d/%d) ")
     command-log
     search-engine
     restclient
     gnus
     graphviz
     systemd
     (lsp :variables
          lsp-ui-sideline-show-code-actions nil
          lsp-headerline-breadcrumb-enable nil
          lsp-enable-file-watchers nil
          lsp-enable-on-type-formatting nil
          lsp-enable-indentation nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-code-actions-enable nil
          lsp-enable-snippet nil
          lsp-enable-symbol-highlighting nil
          lsp-eldoc-enable-hover nil
          lsp-signature-render-documentation nil
          lsp-ui-doc-enable nil
          lsp-ui-doc-show-with-cursor nil
          lsp-ui-doc-show-with-mouse nil
          lsp-ui-sideline-enable nil)
     (treemacs :variables treemacs-position 'right)
     docker
     nginx
     vimscript
     (auto-completion :variables
                      auto-completion-use-company-box t)
     better-defaults
     ;; semantic
     imenu-list
     ibuffer
     asm
     gpu
     cmake
     (c-c++ :variables
            ccls-executable "~/github/ccls/Release/ccls"
            ccls-initialization-options `(:cache (:directory ,(expand-file-name "~/.cache/.ccls-cache"))
                                                 :index (:threads 2))
            c-c++-backend 'lsp-ccls
            c-c++-enable-c++11 t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     (chinese :variables
              ;; pyim-default-scheme 'wubi
              chinese-default-input-method 'wubi)
     (clojure :variables clojure-enable-fancify-symbols nil)
     emacs-lisp
     scheme
     (go :variables go-tab-width 4)
     sml
     haskell
     ;; ocaml
     ;; nixos
     ;; (julia :variables julia-backend 'lsp)
     idris
     octave
     (lua :variables lua-indent-level 4)
     (rust :variables lsp-rust-server 'rust-analyzer)
     ;; (scala :variables
     ;;        scala-backend 'scala-metals
     ;;        scala-indent:use-javadoc-style t
     ;;        scala-enable-eldoc t
     ;;        ;; scala-use-unicode-arrows t
     ;;        ;; scala-auto-start-ensime t
     ;;        scala-auto-insert-asterisk-in-comments t)
     ;; emoji
     ;; games
     ;; dash
     debug
     git
     (github :variables
             forge-topic-list-limit '(15 . 5))
     (version-control :variables
                      version-control-diff-side 'left
                      version-control-global-margin nil)
     ;; (gtags :variables gtags-enable-by-default nil)
     html
     ;; csharp
     java
     groovy
     ;; javascript
     csv
     latex
     markdown
     ;; (org :variables org-enable-reveal-js-support t)
     org
     (python :variables
             python-backend 'lsp
             python-formatter 'yapf
             python-shell-interpreter-args "-i")
     ;; ipython-notebook
     ranger
     sql
     ;; fasd
     (shell :variables
            term-buffer-maximum-size 30000
            vterm-max-scrollback 10000
            shell-default-shell (if (eq system-type 'windows-nt) 'eshell 'vterm)
            shell-default-term-shell "zsh"
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     yaml
     ;; syntax-checking
     fengqi
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(json-mode
                                      separedit
                                      embark
                                      orderless
                                      s
                                      f
                                      dash
                                      ctable
                                      deferred
                                      epc
                                      memory-usage
                                      (dts-mode :location (recipe :fetcher github :repo "feng-qi/dts-mode"))
                                      bing-dict
                                      dictionary
                                      ;; pyim-wbdict
                                      rg
                                      fd-dired
                                      ;; all-the-icons-dired
                                      ;; diredfl
                                      dired-rsync
                                      dired-subtree
                                      dired-narrow
                                      dired-filter
                                      presentation
                                      org-tree-slide
                                      kconfig-mode
                                      ;; protobuf-mode
                                      ;; (targets :location (recipe :fetcher github :repo "noctuid/targets.el"))
                                      exec-path-from-shell)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(chinese-pyim
                                    chinese-word-at-point
                                    treemacs-icons-dired
                                    smartparens
                                    string-edit
                                    vi-tilde-fringe
                                    evil-escape
                                    evil-tutor
                                    coffee-mode
                                    clean-aindent-mode
                                    google-translate
                                    lorem-ipsum
                                    link-hint
                                    move-text
                                    ws-butler
                                    dired-quick-sort
                                    editorconfig
                                    smex
                                    gh-md
                                    multiple-cursors
                                    hungry-delete
                                    rtags
                                    ycmd
                                    company-ycmd
                                    flycheck-ycmd
                                    ace-pinyin
                                    find-by-pinyin-dired
                                    fancy-battery
                                    pangu-spacing
                                    spaceline
                                    clojure-cheatsheet
                                    org-brain
                                    org-pomodoro
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

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 10

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings t
                                       hybrid-mode-default-state 'normal)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
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

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom :separator nil :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font `("Victor Mono"
                               :size ,(if (string= system-name "qi-manjaro") 15 13)
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

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
   dotspacemacs-loading-progress-bar nil

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
   dotspacemacs-mode-line-unicode-symbols nil

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

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
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

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

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
  ;; (setq tramp-ssh-controlmaster-options
  ;;       "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
          ("org-cn"   . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/org/")
          ("gnu-cn"   . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")))
  ;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

  ;; set transparency background when started from terminal
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))

  (add-hook 'window-setup-hook 'on-after-init)
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
  (unless (eq system-type 'windows-nt)
    (exec-path-from-shell-initialize)
    (setenv "BASH_ENV" (expand-file-name "~/.bashrc")))
  (when (eq system-type 'darwin)
    (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
    (setq python-shell-interpreter-args ""
          mac-option-modifier  'meta
          ns-function-modifier 'control
          mac-command-modifier 'meta))
  (global-company-mode 1)
  (global-so-long-mode 1)
  (size-indication-mode 1)
  (electric-pair-mode 1)
  (evil-set-undo-system 'undo-redo)
  (setq hscroll-step                  1
        comp-async-report-warnings-errors nil
        time-stamp-format             "%Y-%02m-%02d %02H:%02M:%02S %5z"
        notes-default-directory       "~/github/notes"
        shell-file-name               "bash"
        tab-always-indent             t
        reb-re-syntax                 'string ; =C-c TAB= in re-builder to switch
        doom-modeline-height          20
        find-file-visit-truename      t
        compilation-window-height     20
        delete-by-moving-to-trash     nil
        sp-highlight-pair-overlay     nil
        sp-highlight-wrap-overlay     nil
        sp-highlight-wrap-tag-overlay nil)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))
        c-doc-comment-style '((java-mode . javadoc)
                              (c-mode    . doxygen)
                              (c++-mode  . doxygen)))
  (setq-default fill-column    80
                truncate-lines t)

  (add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.ad\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.def\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\.[0-9]\\{3\\}t\\.[[:alnum:]-_]+\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\.[0-9]\\{3\\}r\\.[[:alnum:]-_]+\\'" . lisp-mode))

  (spacemacs/set-leader-keys
    (kbd "8")   'spacemacs/toggle-maximize-frame
    (kbd "RET") 'fengqi/eshell-command
    (kbd "bv")  'mark-whole-buffer
    (kbd "by")  'spacemacs/copy-whole-buffer-to-clipboard
    (kbd "ce")  'fengqi/show-hide-async-shell-command-window
    (kbd "cs")  'fengqi/untabify-region-or-buffer
    (kbd "ct")  'fengqi/tabify-region-or-buffer
    (kbd "fCc") 'set-buffer-file-coding-system ; change buffer encoding
    (kbd "fCr") 'revert-buffer-with-coding-system
    (kbd "fd")  'fd-dired
    (kbd "fF")  'fengqi/search-for-file
    (kbd "iv")  'rectangle-number-lines ; https://www.reddit.com/r/emacs/comments/3n1ikz/turn_column_of_0s_into_incrementing_values/
    (kbd "nn")  'fengqi/narrow-to-region-or-defun
    (kbd "nw")  'widen
    (kbd "oc")  'fengqi/show-different-radix
    (kbd "od")  'dictionary-search
    (kbd "oeb") 'fengqi/eval-region-or-line-by-bash
    (kbd "oep") 'fengqi/eval-region-or-line-by-python
    (kbd "oez") 'fengqi/eval-region-or-line-by-zsh
    (kbd "ogg") 'fengqi/magit-status-add-dotfiles-config
    (kbd "ogd") 'fengqi/magit-status-remove-dotfiles-config
    (kbd "oi")  'fengqi/generate-number-sequence
    (kbd "op")  'plur-replace
    (kbd "oq")  'fengqi/qrencode-from-region-or-clipboard
    (kbd "ot")  'fengqi/touch-current-buffer-file-now
    (kbd "ov")  'fengqi/calc-on-rectangle
    (kbd "ow")  'delete-trailing-whitespace
    (kbd "pf")  'counsel-git
    (kbd "qQ")  'spacemacs/kill-emacs
    (kbd "qq")  'fengqi/delete-frame
    (kbd "sn")  'fengqi/search-notes
    (kbd "ss")  'fengqi/swiper
    (kbd "te")  'fengqi/toggle-lsp-signature-render-documentation
    (kbd "wo")  'spacemacs/toggle-maximize-buffer
    (kbd "ws")  'split-window-below-and-focus
    (kbd "wv")  'split-window-right-and-focus
    ;; (kbd "y")   'fengqi/kill-and-osc52-send
    (kbd "xas") 'fengqi/aligh-repeat-whitespace)

  (with-eval-after-load 'embark
    (setq embark-action-indicator
          (lambda (map _target)
            (which-key--show-keymap "Embark" map nil nil 'no-paging)
            #'which-key--hide-popup-ignore-command)
          embark-become-indicator embark-action-indicator)
    (fengqi/define-key embark-general-map
                       "c"   #'fengqi/calc-eval)
    (fengqi/define-key embark-region-map
                       "e"   #'eval-and-replace
                       "c"   #'fengqi/calc-eval)
    (fengqi/define-key embark-file-map
                       "e"   #'eaf-open)
    (fengqi/define-key embark-url-map
                       "e"   #'eaf-open-browser))

  (with-eval-after-load 'cc-mode
    (fengqi/define-key c++-mode-map
                       (kbd "M-e") #'symbol-overlay-put)
    (dolist (mode '(c-mode c++-mode))
      (evil-leader/set-key-for-mode mode
        "ff" 'spacemacs/clang-format-region-or-buffer
        "oo" 'fengqi/set-compile-command)))

  (add-hook 'lsp-mode-hook
            (lambda () (evil-leader/set-key-for-mode major-mode
                         ","  'lsp-ui-peek-find-definitions
                         "."  'lsp-ui-peek-find-references)))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-engine 'xetex)
              (add-to-list 'TeX-command-list
                           '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (defun arm-asm-setup () (progn (modify-syntax-entry ?/ ". 12") (modify-syntax-entry ?\; ".") (setq comment-start "//")))
  ;; (add-hook 'nasm-mode-hook #'arm-asm-setup)
  ;; (add-hook 'asm-mode-hook #'arm-asm-setup)

  (with-eval-after-load "org"
    (add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t) (auto-fill-mode t)))
    (org-babel-do-load-languages
     'org-babel-load-languages '((shell . t)
                                 (calc . t)
                                 (python . t)))
    ;; (add-to-list 'org-modules 'org-tempo)
    ;; (add-to-list 'org-export-backends 'md)
    ;; (org-tree-slide-simple-profile)
    (fengqi/define-key org-mode-map
                       ;; (kbd "<f8>")   #'org-tree-slide-mode
                       ;; (kbd "<f9>")   #'org-tree-slide-move-next-tree
                       (kbd "C-c ,")  #'org-insert-structure-template)
    (setq org-export-with-section-numbers  nil
          org-export-with-sub-superscripts nil
          org-src-window-setup 'split-window-below
          org-link-frame-setup '((vm      . vm-visit-folder-other-frame)
                                 (vm-imap . vm-visit-imap-folder-other-frame)
                                 (gnus    . org-gnus-no-new-news)
                                 (file    . find-file)
                                 (wl      . wl-other-frame))
          org-confirm-babel-evaluate (lambda (lang body)
                                       (not (member lang '("sh" "bash" "shell" "python" "elisp"))))
          org-agenda-files '("~/org")
          ;; org-pomodoro-length 40
          ;; org-hide-leading-stars t
          org-capture-templates '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
                                   "* TODO [#A] %?\nSCHEDULED: %T\n  %a\n")
                                  ("?" "Question" entry (file+headline "~/org/questions.org" "Questions")
                                   "* QUESTION [#B] %?\nSCHEDULED: %T\n  %a\n" :prepend t)
                                  ("w" "Work schedules" plain (file+olp+datetree "~/org/work-schedules.org")
                                   "**** TODO %?\n     SCHEDULED: %T\n     %a\n"))))
  (with-eval-after-load "realgud"
    (fengqi/define-key realgud:shortkey-mode-map
                       (kbd "'") #'evil-goto-mark-line
                       (kbd "0") #'evil-digit-argument-or-evil-beginning-of-line
                       (kbd "e") #'realgud:cmd-eval-at-point
                       (kbd "E") #'realgud:cmd-eval-dwim
                       (kbd "f") #'evil-find-char
                       (kbd "m") #'evil-set-marker
                       (kbd "n") #'realgud:cmd-next
                       (kbd "t") #'evil-find-char-to
                       (kbd "w") #'evil-forward-word-begin
                       (kbd "zt") #'evil-scroll-line-to-top
                       (kbd "zz") #'evil-scroll-line-to-center
                       (kbd "zb") #'evil-scroll-line-to-bottom))
  (with-eval-after-load "treemacs"
    (setq treemacs-position 'right))
  (with-eval-after-load "dired"
    ;; (diredfl-global-mode)
    ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    (evil-define-key 'normal dired-mode-map
      (kbd "TAB")   'dired-subtree-toggle
      "i"   'fengqi/dired-toggle-read-only-in-evil-normal
      "o"   'dired-find-file-other-window
      "r"   'dired-rsync
      "s"   'fengqi/dired-sort-other
      "h"   'dired-subtree-up
      "H"   'evil-window-top
      "L"   'evil-window-bottom
      "K"   'dired-subtree-previous-sibling
      "J"   'dired-subtree-next-sibling)
    (add-hook 'dired-mode-hook 'dired-filter-mode))
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))
  (with-eval-after-load 'magit
    (evil-define-key 'normal magit-status-mode-map
      "[[" #'magit-section-backward-sibling
      "]]" #'magit-section-forward-sibling)
    (setq magit-log-margin-show-committer-date t)
    (define-key magit-status-mode-map (kbd "M-RET") #'forge-browse-dwim)
    (define-key magit-status-mode-map (kbd "%") #'magit-worktree))
  (with-eval-after-load 'rg
    (rg-define-search fengqi/search-for-file
      :files "everything"
      :dir ask
      :confirm always
      :flags ("--files"))
    (define-key rg-mode-map (kbd "?") #'rg-menu))
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  ;; pyim-wbdict setup
  ;; (pyim-wbdict-v98-enable)

  (spacemacs|define-custom-layout "@Shell"
    :binding "s"
    :body
    (vterm))

  (with-eval-after-load 'eshell
    (defun eshell-find-alias-function (name)
      "Check whether a function called `eshell/NAME' exists."
      (let ((eshell-disabled-alias-list '("date")))
        (unless (member name eshell-disabled-alias-list)
          (intern-soft (concat "eshell/" name)))))

    (defun eshell/take (&rest args)
      "cd or create directory when necessary."
      (let ((directory-name (car args)))
        (cond ((file-directory-p directory-name) (eshell/cd directory-name))
              ((file-exists-p directory-name)    (message "Error: target is not a directory"))
              (t (progn (eshell/mkdir "-p" directory-name)
                        (eshell/cd directory-name)))))))
  (with-eval-after-load 'em-hist
    (define-key eshell-hist-mode-map
      (kbd "C-r")  #'counsel-esh-history))
  (with-eval-after-load 'calendar
    (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
    (setq calendar-chinese-all-holidays-flag t))

  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode -1))

  (autoload 'eaf-open "eaf" "eaf-open" t)
  (autoload 'eaf-open-browser "eaf" "eaf-open-browser" t)
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework")
  (with-eval-after-load 'eaf
    ;; (require 'eaf)
    (require 'eaf-evil)
    (require 'eaf-org)
    (eaf-bind-key scroll_up      "e" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up_page "f" eaf-pdf-viewer-keybinding)
    (eaf-bind-key jump_to_link   "F" eaf-pdf-viewer-keybinding)
    (eaf-bind-key insert_or_scroll_up        "e" eaf-browser-keybinding)
    (eaf-bind-key insert_or_scroll_up_page   "f" eaf-browser-keybinding)
    (eaf-bind-key insert_or_scroll_down_page "b" eaf-browser-keybinding)
    (eaf-bind-key insert_or_open_link        "F" eaf-browser-keybinding)
    (eaf-bind-key toggle_play "p" eaf-js-video-player-keybinding)
    (eaf-bind-key toggle_play "C-SPC" eaf-js-video-player-keybinding)
    (setq eaf-evil-leader-key    "SPC"
          eaf-evil-leader-keymap 'spacemacs-cmds))
  (setq browse-url-handlers
        '(("\\`https://github.com" . eaf-open-browser)
          ("\\`file:" . browse-url-default-browser)))

  (define-key prog-mode-map (kbd "C-c '") #'separedit)

  (defun orderless//flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun orderless//without-if-bang (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun orderless//literal-if-equal (pattern _index _total)
    (when (string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1))))
  (setq orderless-component-separator 'orderless-escapable-split-on-space
        ivy-re-builders-alist         '((t . orderless-ivy-re-builder))
        orderless-matching-styles     '(orderless-regexp)
        orderless-style-dispatchers   '(orderless//without-if-bang
                                        orderless//literal-if-equal
                                        orderless//flex-if-twiddle))

  (ivy-set-actions
   'counsel-find-file
   '(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("c" counsel-find-file-copy "copy file")
     ("d" counsel-find-file-delete "delete")
     ("e" eaf-open "eaf-open")
     ("i" ivy--action-insert "insert")
     ("k" counsel-find-file-mkdir-action "mkdir")
     ("l" find-file-literally "open literally")
     ("m" counsel-find-file-move "move or rename")
     ("t" fengqi/touch-file-now "touch")
     ("w" ivy--action-copy "copy current string")
     ("x" fengqi/async-run-file "execute file")))

  (evil-define-key '(normal visual) 'global
    (kbd "C-t")   'spacemacs/jump-to-last-layout
    (kbd "+")     'evil-numbers/inc-at-pt
    (kbd "-")     'evil-numbers/dec-at-pt
    (kbd "M-.")   'fengqi/bing-dict-search)

  (fengqi/define-key-for-keymaps
   '((global-map
      (kbd "M-a") 'embark-act
      (kbd "C-s") 'isearch-forward
      (kbd "M-u") 'fengqi/upcase-region-or-symbol-at-point
      (kbd "M-l") 'fengqi/downcase-region-or-symbol-at-point
      (kbd "M-c") 'fengqi/capitalize-region-or-symbol-at-point
      (kbd "M-.") 'bing-dict-brief-at-point
      (kbd ";")   'self-insert-command
      (kbd "RET") 'newline)
     (symbol-overlay-map
      (kbd "n") #'symbol-overlay-jump-next
      (kbd "p") #'symbol-overlay-jump-prev
      (kbd "r") #'symbol-overlay-rename
      (kbd "t") #'symbol-overlay-toggle-in-scope)
     (evil-normal-state-map
      (kbd "M-e") 'symbol-overlay-put
      (kbd "M-n") 'symbol-overlay-switch-forward
      (kbd "M-p") 'symbol-overlay-switch-backward)
     (evil-ex-completion-map
      (kbd "C-f") #'forward-char
      (kbd "C-b") #'backward-char
      (kbd "C-a") #'beginning-of-line)))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(setq custom-file (expand-file-name "customize-group-setup.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
