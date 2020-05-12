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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     (ivy :variables
          ivy-count-format "(%d/%d) ")
     command-log
     (search-engine :variables
                    browse-url-browser-function 'browse-url-generic
                    engine/browser-function 'browse-url-generic
                    browse-url-generic-program "firefox")
     restclient
     gnus
     graphviz
     systemd
     (lsp :variables
          lsp-enable-file-watchers nil
          lsp-ui-doc-enable nil
          lsp-ui-sideline-enable nil)
     (treemacs :variables treemacs-position 'right)
     docker
     nginx
     vimscript
     auto-completion
     better-defaults
     ;; semantic
     imenu-list
     ibuffer
     asm
     gpu
     cmake
     (c-c++ :variables
            ccls-executable "~/github/ccls/Release/ccls"
            ccls-initialization-options '(:index (:threads 2))
            c-c++-backend 'lsp-ccls
            c-c++-enable-c++11 t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     (chinese :variables
              pyim-default-scheme 'wubi
              chinese-enable-youdao-dict t)
     (clojure :variables clojure-enable-fancify-symbols nil)
     emacs-lisp
     scheme
     parinfer
     (go :variables go-tab-width 4)
     sml
     haskell
     idris
     octave
     (lua :variables lua-indent-level 4)
     rust
     (scala :variables
            scala-indent:use-javadoc-style t
            scala-enable-eldoc t
            ;; scala-use-unicode-arrows t
            ;; scala-auto-start-ensime t
            scala-auto-insert-asterisk-in-comments t)
     ;; emoji
     ;; games
     ;; dash
     debug
     git
     ;; github
     (version-control :variables
                      version-control-diff-side 'left
                      version-control-global-margin nil)
     ;; (gtags :variables gtags-enable-by-default nil)
     html
     csharp
     java
     groovy
     ;; javascript
     csv
     latex
     markdown
     ;; (org :variables org-enable-reveal-js-support t)
     org
     pdf
     (python :variables
             python-backend 'lsp
             flycheck-python-pycompile-executable "python3"
             python-shell-interpreter "python3"
             python-shell-interpreter-args "-i")
     ipython-notebook
     ranger
     sql
     ;; fasd
     (shell :variables
            term-buffer-maximum-size 30000
            vterm-max-scrollback 10000
            shell-default-shell 'vterm
            shell-default-term-shell "zsh"
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     yaml
     ;; syntax-checking
     fengqi
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(json-mode
                                      memory-usage
                                      bing-dict
                                      dictionary
                                      pyim-wbdict
                                      ;; fd-dired
                                      all-the-icons-dired
                                      ;; diredfl
                                      dired-subtree
                                      dired-narrow
                                      dired-filter
                                      presentation
                                      org-tree-slide
                                      exec-path-from-shell
                                      ;; (targets :location (recipe :fetcher github :repo "noctuid/targets.el"))
                                      protobuf-mode)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(chinese-pyim
                                    chinese-word-at-point
                                    vi-tilde-fringe
                                    evil-escape
                                    evil-tutor
                                    coffee-mode
                                    clean-aindent-mode
                                    google-translate
                                    lorem-ipsum
                                    gh-md
                                    multiple-cursors
                                    ace-pinyin
                                    find-by-pinyin-dired
                                    fancy-battery
                                    pangu-spacing
                                    spaceline
                                    clojure-cheatsheet
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
   dotspacemacs-elpa-timeout 10

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
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         zenburn
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

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font `("Source Code Pro"
                               :size ,(if (string= system-name "qi-laptop") 15 13)
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

   ;; Code folding method. Possible values are `evil' and `origami'.
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
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
          ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
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
    ;; (exec-path-from-shell-initialize)
    (setenv "BASH_ENV" (expand-file-name "~/.bashrc")))
  (when (eq system-type 'darwin)
    (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
    (setq python-shell-interpreter-args ""
          mac-option-modifier  'meta
          ns-function-modifier 'control
          mac-command-modifier 'meta))
  (global-company-mode t)
  (setq hscroll-step                  1
        doom-modeline-height          20
        find-file-visit-truename      t
        compilation-window-height     20
        delete-by-moving-to-trash     nil
        sp-highlight-pair-overlay     nil
        sp-highlight-wrap-overlay     nil
        sp-highlight-wrap-tag-overlay nil)

  (add-to-list 'auto-mode-alist '("\\.ad\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.def\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'"   . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\.[0-9]\\{3\\}t\\.[[:alnum:]-_]+\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\.[0-9]\\{3\\}r\\.[[:alnum:]-_]+\\'" . lisp-mode))

  (spacemacs/set-leader-keys
    (kbd "8")   'spacemacs/toggle-maximize-frame
    (kbd "bv")  'mark-whole-buffer
    (kbd "by")  'spacemacs/copy-whole-buffer-to-clipboard
    (kbd "cs")  'fengqi/untabify-region-or-buffer
    (kbd "ct")  'fengqi/tabify-region-or-buffer
    (kbd "fCc") 'set-buffer-file-coding-system ; change buffer encoding
    (kbd "fCr") 'revert-buffer-with-coding-system
    (kbd "iv")  'rectangle-number-lines ; https://www.reddit.com/r/emacs/comments/3n1ikz/turn_column_of_0s_into_incrementing_values/
    (kbd "nn")  'fengqi/narrow-to-region-or-defun
    (kbd "nw")  'widen
    (kbd "odd") 'dictionary-search
    (kbd "odb") 'bing-dict-brief
    (kbd "oec") 'fengqi/calc-eval
    (kbd "oee") 'eval-and-replace
    (kbd "oeb") 'fengqi/eval-region-or-line-by-bash
    (kbd "oep") 'fengqi/eval-region-or-line-by-python3
    (kbd "oez") 'fengqi/eval-region-or-line-by-zsh
    (kbd "oi")  'fengqi/generate-number-sequence
    (kbd "op")  'plur-replace
    (kbd "oq")  'fengqi/qrencode-from-region-or-clipboard
    (kbd "ot")  (lambda () (interactive) (fengqi/touch-file-now (buffer-file-name)))
    (kbd "ov")  'fengqi/calc-on-rectangle
    (kbd "pf")  'counsel-git
    (kbd "qQ")  'spacemacs/kill-emacs
    (kbd "qq")  'fengqi/delete-frame
    (kbd "toi") 'org-toggle-inline-images
    (kbd "wo")  'spacemacs/toggle-maximize-buffer
    (kbd "ws")  'split-window-below-and-focus
    (kbd "wv")  'split-window-right-and-focus
    ;; (kbd "y")   'fengqi/kill-and-osc52-send
    (kbd "xas") 'fengqi/aligh-repeat-whitespace)

  (with-eval-after-load 'cc-mode
    (fengqi/define-key c++-mode-map
                       (kbd "M-e") #'symbol-overlay-put)
    (dolist (mode '(c-mode c++-mode))
      (evil-leader/set-key-for-mode mode
        ","  'lsp-ui-peek-find-definitions
        "."  'lsp-ui-peek-find-implementation
        "ff" 'spacemacs/clang-format-region-or-buffer
        "oo" 'fengqi/set-compile-command)))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-engine 'xetex)
              (add-to-list 'TeX-command-list
                           '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (defun arm-asm-setup () (progn (modify-syntax-entry ?/ ". 12") (modify-syntax-entry ?\; ".") (setq comment-start "//")))

  ;; (fengqi/define-key org-mode-map
  ;;                    (kbd "<f10>")  #'org-tree-slide-move-previous-tree
  ;;                    (kbd "<f9>")   #'org-tree-slide-move-next-tree
  ;;                    (kbd "<f8>")   #'org-tree-slide-mode)
  ;; (org-tree-slide-simple-profile)
  (with-eval-after-load "org"
    (add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t) (auto-fill-mode t)))
    (org-babel-do-load-languages
     'org-babel-load-languages '((shell . t)
                                 (calc . t)
                                 (python . t)))
    (add-to-list 'org-modules 'org-tempo)
    (add-to-list 'org-export-backends 'ox-md)
    (setq org-export-with-section-numbers  nil
          org-export-with-sub-superscripts nil
          org-link-frame-setup '((vm      . vm-visit-folder-other-frame)
                                 (vm-imap . vm-visit-imap-folder-other-frame)
                                 (gnus    . org-gnus-no-new-news)
                                 (file    . find-file)
                                 (wl      . wl-other-frame))
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
    (add-hook 'dired-mode-hook 'dired-filter-mode))
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (setq c-default-style "k&r")
  (setq-default tab-width      4
                fill-column    72
                truncate-lines t
                c-basic-offset 4)

  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  ;; pyim-wbdict setup
  (pyim-wbdict-v98-enable)

  (spacemacs|define-custom-layout "@Shell"
    :binding "s"
    :body
    (vterm))

  (ivy-set-actions
   'counsel-find-file
   '(("t" fengqi/touch-file-now "touch")))

  (fengqi/define-key-for-keymaps
   '((global-map
      (kbd "C-s") 'isearch-forward
      (kbd "M-u") 'fengqi/upcase-region-or-symbol-at-point
      (kbd "M-l") 'fengqi/downcase-region-or-symbol-at-point
      (kbd "M-c") 'fengqi/capitalize-region-or-symbol-at-point
      (kbd "M-.") 'bing-dict-brief-at-point
      (kbd "RET") 'newline)
     (dired-mode-map
      (kbd "TAB")     'dired-subtree-toggle
      (kbd "<tab>")   'dired-subtree-toggle
      (kbd "S-TAB")   'dired-subtree-cycle
      (kbd "<S-tab>") 'dired-subtree-cycle
      (kbd "i")       'fengqi/dired-toggle-read-only-in-evil-normal
      (kbd "s")       'fengqi/dired-sort-other
      (kbd "f")       'evil-find-char)
     (symbol-overlay-map
      (kbd "n") #'symbol-overlay-jump-next
      (kbd "p") #'symbol-overlay-jump-prev
      (kbd "r") #'symbol-overlay-rename
      (kbd "t") #'symbol-overlay-toggle-in-scope)
     (evil-visual-state-map
      (kbd "is") 'evil-inner-symbol
      (kbd "+")  'evil-numbers/inc-at-pt
      (kbd "-")  'evil-numbers/dec-at-pt)
     (evil-normal-state-map
      (kbd "M-e") 'symbol-overlay-put
      (kbd "M-i") 'symbol-overlay-put
      (kbd "M-n") 'symbol-overlay-switch-forward
      (kbd "M-p") 'symbol-overlay-switch-backward
      (kbd "g[")  'backward-page
      (kbd "g]")  'forward-page
      (kbd "+")   'evil-numbers/inc-at-pt
      (kbd "-")   'evil-numbers/dec-at-pt
      (kbd "M-.") 'bing-dict-brief-at-point)
     (evil-hybrid-state-map
      (kbd "M-.") 'bing-dict-brief-at-point)))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(setq custom-file (expand-file-name "customize-group-setup.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
