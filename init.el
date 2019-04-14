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
     command-log
     (search-engine :variables
                    browse-url-browser-function 'browse-url-generic
                    engine/browser-function 'browse-url-generic
                    browse-url-generic-program "firefox")
     restclient
     deft
     gnus
     graphviz
     systemd
     lsp
     (treemacs :variables treemacs-position 'right)
     docker
     nginx
     vimscript
     auto-completion
     better-defaults
     ;; semantic
     imenu-list
     asm
     cmake
     (c-c++ :variables
            c-c++-enable-c++11 t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     (chinese :variables
              chinese-default-input-method 'wubi
              chinese-enable-youdao-dict t)
     (clojure :variables clojure-enable-fancify-symbols nil)
     ;; ivy
     emacs-lisp
     parinfer
     (go :variables go-tab-width 4)
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
     git
     ;; github
     (version-control :variables
                      version-control-diff-side 'left
                      version-control-global-margin nil)
     ;; (gtags :variables gtags-enable-by-default nil)
     html
     csharp
     java
     ;; javascript
     latex
     markdown
     ;; (org :variables org-enable-reveal-js-support t)
     org
     pdf
     (python :variables python-backend 'anaconda)
     ;; ipython-notebook
     ranger
     sql
     ;; fasd
     (shell :variables
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
                                      fd-dired
                                      all-the-icons-dired
                                      dired-subtree
                                      dired-narrow
                                      dired-filter
                                      presentation
                                      org-tree-slide
                                      evil-search-highlight-persist
                                      exec-path-from-shell
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
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings t
                                       hybrid-mode-default-state 'normal)
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

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

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
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
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
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
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
          ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq byte-compile-warnings '(not obsolete))
  ;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq org-agenda-files '("~/org"))
  (setq org-pomodoro-length 40)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

  ;; set transparency background when started from terminal
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (load-theme 'spacemacs-dark)
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
  (exec-path-from-shell-initialize)
  (when (eq system-type 'darwin)
    (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
    (setq mac-option-modifier  'alt
          ns-function-modifier 'control
          mac-command-modifier 'meta))
  (global-company-mode t)
  (global-evil-search-highlight-persist t)
  (setq-default python-shell-interpreter "python3")
  (setq hscroll-step                  1
        deft-directory                "~/github/notes"
        doom-modeline-height          20
        find-file-visit-truename      t
        sp-highlight-pair-overlay     nil
        sp-highlight-wrap-overlay     nil
        sp-highlight-wrap-tag-overlay nil)

  (add-to-list 'auto-mode-alist '("\\.def\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'"   . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\.[0-9]\\{3\\}t\\.[[:alnum:]-_]+\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\.[0-9]\\{3\\}r\\.[[:alnum:]-_]+\\'" . lisp-mode))

  (with-eval-after-load "evil"
    (fengqi/define-key evil-normal-state-map
                       "g[" 'backward-page ; https://github.com/lujun9972/emacs-document/blob/master/emacs-common/%E5%90%88%E7%90%86%E5%9C%B0%E5%9C%A8Emacs%E4%B8%AD%E4%BD%BF%E7%94%A8%E5%88%86%E9%A1%B5%E7%AC%A6.org
                       "g]" 'forward-page
                       "gu" 'evil-upcase
                       "gl" 'evil-downcase
                       "+" 'evil-numbers/inc-at-pt
                       "-" 'evil-numbers/dec-at-pt)
    (fengqi/define-key evil-visual-state-map
                       "i9" #'evil-inner-paren
                       "i0" #'evil-inner-paren
                       "a9" #'evil-a-paren
                       "a0" #'evil-a-paren
                       "is" #'evil-inner-symbol
                       "+" 'evil-numbers/inc-at-pt
                       "-" 'evil-numbers/dec-at-pt))
  (with-eval-after-load "symbol-overlay"
    (fengqi/define-key symbol-overlay-map
                       (kbd "d") nil
                       (kbd "e") nil
                       (kbd "h") nil
                       (kbd "i") nil
                       (kbd "q") nil
                       (kbd "w") nil))
  (with-eval-after-load "term"
    (fengqi/define-key term-raw-map
                       (kbd "C-c C-y") #'term-paste))

  (global-set-key (kbd "C-c i") 'ido-insert-buffer)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-e") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "M-u") 'fengqi/upcase-region-or-symbol-at-point)
  (global-set-key (kbd "M-l") 'fengqi/downcase-region-or-symbol-at-point)
  (spacemacs/set-leader-keys
    (kbd "8")   'spacemacs/toggle-maximize-frame
    (kbd "bv")  'mark-whole-buffer
    (kbd "by")  'spacemacs/copy-whole-buffer-to-clipboard
    (kbd "cs")  'fengqi/untabify-region-or-buffer
    (kbd "ct")  'fengqi/tabify-region-or-buffer
    (kbd "dc")  'fengqi/describe-buffer-file-coding-system
    (kbd "dw")  'delete-trailing-whitespace
    (kbd "fCc") 'set-buffer-file-coding-system ; change buffer encoding
    (kbd "fCr") 'revert-buffer-with-coding-system
    (kbd "fd")  'fd-dired
    (kbd "iv")  'rectangle-number-lines ; https://www.reddit.com/r/emacs/comments/3n1ikz/turn_column_of_0s_into_incrementing_values/
    (kbd "nn")  'fengqi/narrow-to-region-or-defun
    (kbd "nw")  'fengqi/widen
    (kbd "oc")  'fengqi/copy-current-buffer-name
    (kbd "od")  'ediff-buffers
    (kbd "oe")  'eval-and-replace
    (kbd "oi")  'fengqi/count-words-region
    (kbd "oo")  'youdao-dictionary-play-voice-at-point
    (kbd "op")  'plur-replace
    (kbd "or")  'fengqi/string-reverse
    (kbd "oy")  'youdao-dictionary-search-at-point+
    (kbd "qQ")  'spacemacs/kill-emacs
    (kbd "qq")  'fengqi/delete-frame
    (kbd "sc")  'fengqi/remove-highlight
    (kbd "toi") 'org-toggle-inline-images
    (kbd "wo")  'spacemacs/toggle-maximize-buffer
    (kbd "ws")  'split-window-below-and-focus
    (kbd "wv")  'split-window-right-and-focus
    (kbd "xas") 'fengqi/aligh-repeat-whitespace)

  (dolist (mode '(c-mode c++-mode))
    (evil-leader/set-key-for-mode mode
      "f f" 'spacemacs/clang-format-region-or-buffer
      "o o" 'fengqi/set-compile-command))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-engine 'xetex)
              (add-to-list 'TeX-command-list
                           '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook (lambda () (progn
                                        (setq show-trailing-whitespace t)
                                        (smartparens-mode t)
                                        (auto-fill-mode t))))

  ;; markdown exporter, more info: https://orgmode.org/worg/exporters/ox-overview.html
  (with-eval-after-load "org"
    ;; (define-key org-mode-map [(kbd "C-'")] nil)
    (fengqi/define-key org-mode-map
                       (kbd "C-'")    #'fengqi/upcase-previous-WORD
                       (kbd "<f10>")  #'org-tree-slide-move-previous-tree
                       (kbd "<f9>")   #'org-tree-slide-move-next-tree
                       (kbd "<f8>")   #'org-tree-slide-mode)
    (org-tree-slide-simple-profile)
    (require 'ox-md nil t)
    ;; (setq org-hide-leading-stars t)
    (setq org-export-with-section-numbers  nil
          org-export-with-sub-superscripts nil)
    (setq org-link-frame-setup
          '((vm      . vm-visit-folder-other-frame)
            (vm-imap . vm-visit-imap-folder-other-frame)
            (gnus    . org-gnus-no-new-news)
            (file    . find-file)
            (wl      . wl-other-frame))))
  (with-eval-after-load "treemacs"
    (setq treemacs-position 'right))
  (with-eval-after-load "dired"
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    (add-hook 'dired-mode-hook 'dired-filter-mode)
    (fengqi/define-key dired-mode-map
                       (kbd "<return>") #'dired-find-alternate-file
                       (kbd "<tab>")    #'dired-subtree-toggle
                       (kbd "<S-tab>")  #'dired-subtree-cycle
                       (kbd "c")        #'spacemacs/new-empty-buffer
                       (kbd "s")        #'fengqi/dired-sort-other
                       (kbd "^")        (lambda () (interactive) (find-alternate-file ".."))))
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

  ;; (add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))

  (setq c-default-style "k&r")
  (setq-default tab-width      4
                fill-column    80
                truncate-lines t
                c-basic-offset 4)

  (put 'helm-make-build-dir 'safe-local-variable 'stringp)


  ;; language server

  ;; Configuration to fix LSP errors.
  (setq lsp-enable-eldoc nil) ;we will got error "Wrong type argument: sequencep" from `eldoc-message' if `lsp-enable-eldoc' is non-nil
  (setq lsp-message-project-root-warning t) ;avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
  (setq create-lockfiles nil) ;we will got error "Error from the Language Server: FileNotFoundError" if `create-lockfiles' is non-nil

  ;; (add-hook 'python-mode-hook 'lsp-python-enable)
  ;; (spacemacs/lsp-bind-keys-for-mode 'python-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(setq custom-file (expand-file-name "customize-group-setup.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
