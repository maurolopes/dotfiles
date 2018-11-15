;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


;; neotree
;; zeal-at-point
;; my-kill-ring-save



(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     chrome
     (clojure :variables
              clojure-enable-fancify-symbols nil
              cider-pprint-fn 'fipp
              cider-repl-display-help-banner nil
              cider-repl-use-pretty-printing t)
     clojure-lint
     csv
     dash
     docker
     emacs-lisp
     ;; extra-langs
     fsharp
     git
     ;;github
     html
     ;;ipython-notebook
     ivy
     javascript
     markdown
     nixos
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t)
     python
     ;;reason
     restclient
     ruby
     rust
     ;;rustrls
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; slack
     speed-reading
     ;; spell-checking
     sql
     syntax-checking
     terraform
     ;; themes-megapack
     typescript
     ;; version-control
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(exec-path-from-shell
     feature-mode
     flycheck-joker
     keyfreq
     nix-mode
     multiple-cursors
     protobuf-mode
     ;;lsp-mode
     ;;lsp-rust
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gruber-darker
                         soft-stone)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "<f19>" ;;"<menu>"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "<f19>" ;;"<menu>"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-n"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
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
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first.")

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	      (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun set-paredit-keybindings ()
  (define-key paredit-mode-map (kbd ")") 'paredit-close-round)

  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)

  ;; (define-key paredit-mode-map (kbd "<C-M-delete>") 'kill-sexp)

  ;; (define-key paredit-mode-map (kbd "C-M-`") 'paredit-raise-sexp)
  ;; (define-key paredit-mode-map (kbd "C-M-1") 'paredit-join-sexps)
  ;; (define-key paredit-mode-map (kbd "C-M-2") 'paredit-split-sexp)
  ;; (define-key paredit-mode-map (kbd "C-M-3") 'paredit-splice-sexp)
  ;; (define-key paredit-mode-map (kbd "C-M-4") 'paredit-splice-sexp-killing-backward)
  ;; (define-key paredit-mode-map (kbd "C-M-5") 'paredit-splice-sexp-killing-forward)

  (define-key paredit-mode-map (kbd "<C-M-up>")    'paredit-backward-up)
  (define-key paredit-mode-map (kbd "<C-M-down>")  'paredit-forward-down)
  (define-key paredit-mode-map (kbd "<C-M-left>")  'paredit-backward)
  (define-key paredit-mode-map (kbd "<C-M-right>") 'paredit-forward)

  (define-key paredit-mode-map (kbd "<C-left>") nil)
  (define-key paredit-mode-map (kbd "<C-right>") nil)

  (define-key paredit-mode-map (kbd "<C-M-left>") nil)
  (define-key paredit-mode-map (kbd "<C-left>") nil)

  (define-key paredit-mode-map (kbd "<C-backspace>") 'paredit-backward-kill-word)

  (define-key paredit-mode-map (kbd "<C-tab>") 'paredit-reindent-defun))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; doesn't work; apparently doesn't override the theme
  (setq powerline-default-separator 'slant)

  (when (memq window-system '(mac ns x))
    ;; https://github.com/abo-abo/swiper/issues/844
    (exec-path-from-shell-initialize))

  ;; Fix bug in query-replace
  ;; https://github.com/syl20bnr/spacemacs/issues/10938
  (setq frame-title-format nil)

  ;;(add-to-list 'load-path "~/reason-emacs")

  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

  ;; paredit
  (when t
    (add-hook 'cider-repl-mode-hook                  #'enable-paredit-mode)
    (add-hook 'clojure-mode-hook                     #'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook                      #'enable-paredit-mode))

  ;;(sp-use-paredit-bindings)

  (define-key smartparens-mode-map (kbd "<C-backspace>") 'backward-kill-word)
  ;;(define-key smartparens-strict-mode-map (kbd "<S-right>") 'sp-forward-sexp)
  ;;(define-key smartparens-strict-mode-map (kbd "<S-left>") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd ")") 'paredit-close-round)


  (define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
    ;; Define exceptions for centered cursor mode.
    (lambda ()
      (let ((dont-recenter-end-of-file '(messages-buffer-mode
                                         cider-repl-mode
                                         inferior-emacs-lisp-mode
                                         slime-repl-mode
                                         shell-mode
                                         inferior-js-mode
                                         jss-console-mode))
            (disable '(minibuffer-inactive-mode)))
        (cond
         ((memq major-mode dont-recenter-end-of-file)
          ;;(message "dont-recenter-end-of-file %s %s" (symbol-name major-mode) (buffer-name))
          (make-local-variable 'ccm-recenter-at-end-of-file)
          (setq ccm-recenter-at-end-of-file nil)
          (centered-cursor-mode +1))
         ((not (memq major-mode disable))
          ;;(message "enable (not in disable) %s %s" (symbol-name major-mode) (buffer-name))
          (make-local-variable 'ccm-recenter-at-end-of-file)
          (setq ccm-recenter-at-end-of-file t)
          (centered-cursor-mode +1))
         ;;(t (message "disable (else)" (symbol-name major-mode) (buffer-name)))
         ))))

  ;;(global-centered-cursor-mode +1)
  (my-global-centered-cursor-mode +1)

  ;;(global-set-key (kbd "C-l") 'avy-goto-char-timer)
  (global-set-key (kbd "C-l") 'avy-goto-word-1 ;'avy-goto-word-or-subword-1
                  )
  (global-set-key (kbd "C-s") 'isearch-forward)

  (global-set-key (kbd "C-f") 'swiper)
  (global-set-key (kbd "C-r") 'query-replace)
  (global-set-key (kbd "C-S-r") 'query-replace-regexp)

  (define-key evil-emacs-state-map (kbd "C-/") 'hippie-expand)
  (add-hook 'company-mode-hook (lambda ()
                                 (define-key company-active-map (kbd "C-/") 'hippie-expand)))

  ;; (setq avy-timeout-seconds 0.3)
  (global-git-commit-mode t)
  (global-set-key (kbd "<S-undo>") 'undo-tree-redo)
  (global-set-key (kbd "C-;") (lambda ()
                                (interactive)
                                (delete-indentation 1))) ; join-line top-down

  (global-set-key (kbd "<home>") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "<end>") 'mwim-end-of-code-or-line)

  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "C-/") 'hippie-expand)
  (define-key undo-tree-map (kbd "C-/") nil)

  ;;(global-set-key (kbd "<C-help>") 'kill-ring-save)
  ;;(global-set-key (kbd "<S-help>") 'yank)
  ;;(global-set-key (kbd "<M-S-help>") 'counsel-yank-pop)
  (global-set-key (kbd "s-V") 'counsel-yank-pop)

  (setq evil-lisp-state-enter-lisp-state-on-command nil)
  (setq delete-old-versions t)

  (add-hook 'paredit-mode-hook #'set-paredit-keybindings)
  (add-hook 'prog-mode-hook (lambda () (define-key prog-mode-map (kbd "RET") 'electrify-return-if-match)))
  (add-hook 'cider-repl-mode-hook (lambda () (centered-cursor-mode -1)))
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  ;;(define-key clojure-mode-map (kbd "RET") 'electrify-return-if-match)

  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

  (add-hook 'rust-mode-hook #'smartparens-strict-mode)
  (add-hook 'restclient-mode-hook #'smartparens-strict-mode)
  ;;(add-hook 'rust-mode-hook #'lsp-rust-enable)

  (with-eval-after-load 'clojure-mode
    (setq clojure--prettify-symbols-alist
          '(("fn" . 955)
            ("lambda" . 955)
            ;; ("<=" . (?· (Br . Bl) ?≤))
            ;; (">=" . (?· (Br . Bl) ?≥))

            ("<=" . (?\s (Br . Bl) ?≤))
            (">=" . (?\s (Br . Bl) ?≥))

            ("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
            ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                           (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                           (Bc . Bl) ?- (Br . Br) ?>))))

    (dolist (sym '(not-join))
      (put-clojure-indent sym :defn)))

  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    (require 'lsp-rust))


  ;; (load-file "~/.slack/nexo")
  ;; (slack-register-team
  ;;  :name "Nexo"
  ;;  :default t
  ;;  :client-id slack-nexo-client-id
  ;;  :client-secret slack-nexo-client-secret
  ;;  :token slack-nexo-token)

  (define-key evil-emacs-state-map (kbd "C-.") 'repeat)
  (define-key evil-emacs-state-map (kbd "C-M-.") 'repeat-complex-command)
  (define-key evil-emacs-state-map (kbd "C-,") 'hs-toggle-hiding)

  (spacemacs/declare-prefix "M" "multiple cursors")
  (spacemacs/set-leader-keys
    ;; Remove when this issue is solved: https://github.com/syl20bnr/spacemacs/issues/9652
    ;; (kbd "/") 'counsel-projectile-ag

    "M a" 'mc/mark-all-dwim
    "M e" 'mc/edit-lines
    "M l" 'mc/insert-letters
    "M m" 'mc/mark-more-like-this-extended
    "M n" 'mc/insert-numbers
    "M r" 'mc/reverse-regions
    "M s" 'mc/sort-regions))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-keys
   (quote
    (111 105 101 110 100 104 119 102 112 121 117 108 103 106 122 120 99 118 109 98 107 97 114 115 116)))
 '(avy-subword-extra-word-chars nil)
 '(ccm-vpos-init (quote (round (* 21 (window-text-height)) 34)))
 '(cider-lein-command "~/bin/lein")
 '(clojure-defun-indents (quote (fact facts)))
 '(compilation-ask-about-save nil)
 '(compilation-read-command nil)
 '(compilation-window-height 10)
 '(compile-command "make")
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path-from-shell-check-startup-files nil)
 '(frame-brackground-mode (quote dark))
 '(global-prettify-symbols-mode t)
 '(js-indent-level 2)
 '(mc/always-run-for-all t)
 '(package-selected-packages
   (quote
    (flycheck-joker tide typescript-mode multiple-cursors treepy graphql merlin keyfreq ac-ispell auto-complete company-nixos-options nix-mode nixos-options feature-mode dash-at-point sesman rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby auto-yasnippet protobuf-mode sql-indent org-mime dockerfile-mode docker tablist docker-tramp terraform-mode hcl-mode ghub let-alist org-category-capture request-deferred clojure-snippets yaml-mode web-beautify livid-mode json-mode json-snatcher json-reformat js2-refactor js-doc company-tern tern coffee-mode utop tuareg caml ocp-indent toml-mode racer flycheck-rust cargo rust-mode wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode fsharp-mode skewer-mode js2-mode simple-httpd company csv-mode autothemer ein deferred idris-mode prop-menu mmm-mode markdown-toc gh-md zeal-at-point spray emojify circe oauth2 websocket gmail-message-mode ham-mode markdown-mode html-to-markdown flymd edit-server counsel-dash helm-dash dash-functional unfill mwim ob-restclient ob-http company-restclient restclient know-your-http-well ox-reveal ox-gfm org-projectile org-present org-pomodoro alert log4e gntp org-download magit-gh-pulls htmlize gnuplot github-search github-clone github-browse-file gist gh marshal logito pcache ht flycheck-pos-tip flycheck company-quickhelp pos-tip fuzzy company-web web-completion-data company-statistics company-anaconda web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode anaconda-mode pythonic clj-refactor inflections edn paredit yasnippet peg cider-eval-sexp-fu cider seq queue clojure-mode smeargle orgit magit-gitflow gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy)))
 '(prettify-symbols-unprettify-at-point t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#e4e4ef" :background "#181818")))))
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
 '(avy-keys
   (quote
    (111 105 101 110 100 104 119 102 112 121 117 108 103 106 122 120 99 118 109 98 107 97 114 115 116)))
 '(avy-subword-extra-word-chars nil)
 '(ccm-vpos-init (quote (round (* 21 (window-text-height)) 34)))
 '(cider-lein-command "~/bin/lein")
 '(clojure-defun-indents (quote (fact facts)))
 '(compilation-ask-about-save nil)
 '(compilation-read-command nil)
 '(compilation-window-height 10)
 '(compile-command "make")
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path-from-shell-check-startup-files nil)
 '(frame-brackground-mode (quote dark))
 '(global-prettify-symbols-mode t)
 '(js-indent-level 2)
 '(mc/always-run-for-all t)
 '(package-selected-packages
   (quote
    (ob-ipython flycheck-joker tide typescript-mode multiple-cursors treepy graphql merlin keyfreq ac-ispell auto-complete company-nixos-options nix-mode nixos-options feature-mode dash-at-point sesman rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby auto-yasnippet protobuf-mode sql-indent org-mime dockerfile-mode docker tablist docker-tramp terraform-mode hcl-mode ghub let-alist org-category-capture request-deferred clojure-snippets yaml-mode web-beautify livid-mode json-mode json-snatcher json-reformat js2-refactor js-doc company-tern tern coffee-mode utop tuareg caml ocp-indent toml-mode racer flycheck-rust cargo rust-mode wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode fsharp-mode skewer-mode js2-mode simple-httpd company csv-mode autothemer ein deferred idris-mode prop-menu mmm-mode markdown-toc gh-md zeal-at-point spray emojify circe oauth2 websocket gmail-message-mode ham-mode markdown-mode html-to-markdown flymd edit-server counsel-dash helm-dash dash-functional unfill mwim ob-restclient ob-http company-restclient restclient know-your-http-well ox-reveal ox-gfm org-projectile org-present org-pomodoro alert log4e gntp org-download magit-gh-pulls htmlize gnuplot github-search github-clone github-browse-file gist gh marshal logito pcache ht flycheck-pos-tip flycheck company-quickhelp pos-tip fuzzy company-web web-completion-data company-statistics company-anaconda web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode anaconda-mode pythonic clj-refactor inflections edn paredit yasnippet peg cider-eval-sexp-fu cider seq queue clojure-mode smeargle orgit magit-gitflow gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy)))
 '(prettify-symbols-unprettify-at-point t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#e4e4ef" :background "#181818")))))
)
