;; Startup time
;; (emacs-init-time) ; 0.8 s

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defvar better-gc-cons-threshold 67108864) ; 64MB
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold
                  file-name-handler-alist file-name-handler-alist-old)
            (makunbound 'file-name-handler-alist-old)
            (garbage-collect)))

(setq site-run-file nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; End of Startup time

;; Avoid garbage collection when using minibuffer
(defun gc-minibuffer-setup-hook ()
  (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

(defun gc-minibuffer-exit-hook ()
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)

;; Straight package manager

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


(use-package no-littering               ; Keep .emacs.d clean
  :defer nil
  :config
  (require 'no-littering)
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))))


(straight-use-package
 '(gcmh
   :type git
   :host github
   :repo "emacsmirror/gcmh"))
(require 'gcmh)
(gcmh-mode 1)

;; Global settings
(blink-cursor-mode t)
(column-number-mode t)
(global-display-line-numbers-mode t)
(global-font-lock-mode t)
(global-subword-mode t)
(electric-pair-mode t)
(set-face-attribute 'default nil :family "Hack" :height 100)
(set-face-attribute 'line-number-current-line nil :inherit 'line-number :foreground "orange")
(defalias 'yes-or-no-p 'y-or-n-p)

(setq auto-package-update-delete-old-versions t)
(setq auto-package-update-hide-results t)
(setq bidi-paragraph-direction 'left-to-right)
(setq column-enforce-column 128)
(setq compilation-ask-about-save nil)
(setq compilation-message-face 'default)
(setq compilation-read-command nil)
(setq compilation-scroll-output 'first-error)
(setq compilation-window-height 10)
(setq create-lockfiles nil)
(setq display-line-numbers-widen t)
(setq enable-recursive-minibuffers t)
(setq fill-column 80)
(setq indent-tabs-mode nil)
(setq indicate-empty-lines t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq kept-new-versions 6)
(setq load-prefer-newer t)
(setq maximum-scroll-margin 0.5)
(setq read-process-output-max (* 1024 1024))
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill t)
(setq scroll-margin 99999)
(setq scroll-preserve-screen-position t)
(setq show-paren-delay 0.0)
(setq show-paren-mode t)
(setq sp-highlight-pair-overlay nil)
(setq use-package-always-ensure t)
(setq use-package-minimum-reported-time 0)
(setq use-package-verbose t)
(setq version-control t)
(setq vlf-tune-enabled nil)

(setq custom-file (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory))

(setq delete-old-versions t)

;; Global keys

(bind-key "C-/" 'hippie-expand)
(bind-key "C-." 'repeat)
(bind-key "C-^" (lambda () (interactive) (delete-indentation t))) ; join-line top-down

(bind-key "M-o" 'other-window)

;; Use ag until there is a fix for these:
;; https://github.com/abo-abo/swiper/issues/2339
;; https://github.com/hlissner/doom-emacs/issues/3038
(bind-key "M-f" 'counsel-ag)

(bind-key "C-x C-r" 'counsel-recentf)
(bind-key "C-x C-l" 'counsel-locate)
(bind-key "C-x C-b" 'ibuffer-jump)

;; Make some buffers immortal
(add-hook 'kill-buffer-query-functions
	  (lambda ()
	    (if (or (eq (current-buffer) (get-buffer "*scratch*"))
		    (eq (current-buffer) (get-buffer "*Messages*")))
		(progn (bury-buffer)
		       nil)
	      t)))

(require 'dired)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lah")
(setq dired-sidebar-subtree-line-prefix "   ")
(setq dired-subtree-line-prefix "   ")

(use-package paradox
  :custom
  (paradox-column-width-package 40)
  (paradox-column-width-star 5)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-github-token t))

(use-package diminish
  :config (diminish 'subword-mode))

(use-package exec-path-from-shell
  :defer nil
  :config
  (when (memq window-system '(mac ns x))
    ;; https://github.com/abo-abo/swiper/issues/844
    (exec-path-from-shell-initialize)))

(server-start) ; for emacsclient

(use-package ag)
(use-package ripgrep)
(use-package default-text-scale)
(use-package hydra)

(use-package ws-butler                  ; Cleanup whitespace
  :defer nil
  :config (ws-butler-global-mode t))

(use-package recentf
  :after (no-littering)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 20)
  (recentf-max-saved-items 500)

  :config (progn
            (add-to-list 'recentf-exclude no-littering-etc-directory)
            (add-to-list 'recentf-exclude no-littering-var-directory)
            (recentf-mode 1)))

(use-package keyfreq
  :init (progn (add-hook 'after-init-hook 'keyfreq-mode t)
               (add-hook 'after-init-hook 'keyfreq-autosave-mode t))
  :custom
  (git-gutter-fr:side 'right-fringe))

(use-package git-gutter-fringe
  :config (global-git-gutter-mode t))

(use-package volatile-highlights
  :diminish nil
  :config (volatile-highlights-mode t))

(use-package which-key
  :defer nil
  :diminish nil
  :init
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.0)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-show-transient-maps t)
  (which-key-side-window-max-height 0.75)
  (which-key-sort-order 'which-key-prefix-then-key-order)

  :config (which-key-mode +1))

(use-package undo-tree
  :diminish nil
  :defer nil
  :bind (("s-z" . undo-tree-undo)       ; MacOS
         ("s-Z" . undo-tree-redo)       ; MacOS
         ("<undo>" . undo-tree-undo)
         ("<S-undo>" . undo-tree-redo)
         ("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  :config (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (define-key undo-tree-map (kbd "C-/") nil)
  :init (global-undo-tree-mode))

(use-package ivy
  :delight ivy-mode
  :defer nil
  :diminish nil
  :bind
  ([remap switch-to-buffer] . counsel-switch-buffer)
  ([remap switch-to-buffer-other-window] . counsel-switch-buffer-other-window)
  :init
  (use-package historian
    :init (historian-mode +1)
    :config
    (use-package ivy-historian
      :config (ivy-historian-mode t)))
  (ivy-mode +1)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-height 20)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  :config
  (ivy-mode +1)
  (define-key ivy-minibuffer-map (kbd "<C-down>") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "<C-up>") 'ivy-previous-history-element))

(use-package ivy-hydra
  :requires (ivy)
  :after (ivy hydra)
  :config (require 'hydra))

(use-package avy
  :bind ("C-n" . avy-goto-word-1) ; avy-goto-subword-1 sometimes hangs
  :custom
  (avy-keys (string-to-list "oiendhwfpyulgjzxcvmbkarst"))
  (avy-subword-extra-word-chars nil)
  :custom-face
  (avy-lead-face ((t (:foreground "#870000" :weight bold)))))

(let ((mcl/zap-up-to-char-last-char-arg ?a))
  (defun mcl/zap-up-to-char (arg char)
    "With prefix, same as zap-up-to-char "
    (interactive "p\ncZap up to char: ")
    (setq mcl/zap-up-to-char-last-char-arg char)
    (zap-up-to-char arg char))
  (defun mcl/zap-up-to-same-char ()
    (interactive)
    ;; TODO redirect to mcl/zap-up-to-char if arg is nil
    (zap-up-to-char 1 mcl/zap-up-to-char-last-char-arg))
  (bind-key "M-z" 'mcl/zap-up-to-char)
  (bind-key "M-Z" 'mcl/zap-up-to-same-char))

(use-package company
  :defer nil
  :diminish nil
  :config (global-company-mode)
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-idle-delay 0.4)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-minimum-width 15))

(use-package company-box
  :requires (company)
  :defer nil
  :after (all-the-icons company)
  :delight (company-box-mode nil company-box)
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-doc-delay 0))

(use-package column-enforce-mode
  :diminish nil
  :hook (()) ;;((prog-mode . column-enforce-mode))
  :config (face-spec-set column-enforce-face '((t (:background "dark-red")))))

(use-package restclient
  :mode "\\.rest$")

(use-package company-restclient
  :requires (company)
  :after (company restclient)
  :hook ((restclient-mode . (lambda () (add-to-list 'company-backends 'company-restclient)))))

(use-package mwim
  :bind (("<home>" . mwim-beginning-of-code-or-line)
         ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package amx         ; better M-x interface -- integrates with Ivy
  :defer nil
  :diminish "amx"
  :requires (ivy)
  :config
  (amx-mode t)
  :custom
  (amx-history-length 32))

(use-package counsel
  :diminish ""
  :custom
  (counsel-ag-base-command "ag --nocolor --nogroup --hidden %s")
  :config (counsel-mode)
  (with-eval-after-load 'helpful
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)))

(use-package projectile
  :diminish ""
  :bind-keymap* ("C-x p" . projectile-command-map)
  :init (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (use-package counsel-projectile
    :defer nil
    :requires (counsel)
    :config
    (add-to-list 'ivy-initial-inputs-alist '(counsel-projectile-switch-project . ""))
    (counsel-projectile-mode t)))

(use-package helpful
  :bind (("C-h a" . helpful-symbol)
         ("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package swiper
  :bind ("C-f" . swiper))

(use-package vlf ; view large files
  :config (require 'vlf-setup))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :custom
  (expand-region-contract-fast-key "DEL"))

(defhydra multiple-cursors-hydra (:columns 3)
  ("l" mc/edit-lines "edit lines" :exit t)
  ("a" mc/mark-all-dwim "mark all dwim" :exit t)
  ("r" mc/mark-all-in-region-regexp "mark all in region with regexp" :exit t)
  ("@" mc/insert-letters "insert letters")
  ("#" mc/insert-numbers "insert numbers")
  ("R" mc/reverse-regions "reverse regions")
  ("s" mc/sort-regions "sort regions")
  ("m" mc/mark-more-like-this-extended "mark more")
  ("q" nil))

(use-package multiple-cursors
  ;; alternative: https://github.com/victorhge/iedit
  ;; TODO: fix some functions
  :after (hydra)
  :bind ("C-x m" . #'multiple-cursors-hydra/body)
  :custom
  (mc/always-run-for-all t))

(use-package rainbow-delimiters
  :defer nil
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package stripe-buffer              ; Add stripes to a buffer
  :init (progn (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
               (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)))

;; Git

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package magit-todos
  :after magit
  :hook (magit-mode-hook . magit-todos-mode))

(defhydra hydra-smerge (:color amaranth :columns 4)
  ("a" smerge-keep-all "Keep all")
  ("b" smerge-keep-base "Keep base")
  ("m" smerge-keep-upper "Keep mine (upper)") ("u" smerge-keep-upper)
  ("o" smerge-keep-lower "Keep other (lower)") ("l" smerge-keep-lower)
  ("n" smerge-next "Next conflict")
  ("p" smerge-prev "Previous conflict")
  ("r" smerge-resolve "Resolve")
  ("q" nil "quit"))
(add-hook 'smerge-mode-hook (lambda () (bind-key "C-c C-h" 'hydra-smerge smerge-mode-map)))

;; End of Git

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)

(use-package paredit
  :diminish "()"
  :init
  (progn
    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (add-hook 'json-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'scheme-mode-hook #'enable-paredit-mode)))


;; Adapted from https://github.com/bnbeckwith/bnb-emacs

(defmacro mcl/toggle-str (setting)
  "Return a string `[X]` if SETTING is active or `[ ]` if not."
  `(if (and (boundp ',setting) ,setting) '[x] '[_]))
(defhydra hydra-toggle (:color amaranth)
  ("c" column-number-mode nil)
  ("e" toggle-debug-on-error nil)
  ("u" toggle-debug-on-quit nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("v" visual-line-mode nil)
  ("V" global-visual-line-mode nil)
  ("r" dired-toggle-read-only nil)
  ("w" whitespace-mode nil)
  ("b" display-battery-mode nil)
  ("g" git-gutter-mode nil) ; does not work
  ("G" global-git-gutter-mode nil)
  ("h" hl-line-mode nil) ("H" global-hl-line-mode nil)
  ("v" visual-line-mode nil) ("V" global-visual-line-mode nil)
  ("n" display-line-numbers-mode nil)
  ("N" global-display-line-numbers-mode nil)
  ("q" nil))
(bind-key "C-x t" 'hydra-toggle/body)


;; Windows/Frames

(defun mcl/turn-current-window-into-frame ()
  "Kill this window after making a separate frame with the current buffer.
From: https://emacs.stackexchange.com/questions/7116/pop-a-window-into-a-frame"
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;; https://github.com/bnbeckwith/bnb-emacs
(defun mcl/vsplit-last-buffer ()
  "Split window vertically, using the previous buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
(defun mcl/hsplit-last-buffer ()
  "Split window horizontally, using the previous buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(use-package winner
  :init (winner-mode))

(use-package rotate ; rotate windows and layouts
  :defer nil)

(defhydra hydra-frame (:color blue)
  ("f" make-frame "Make new frame")
  ("d" delete-frame "delete current frame")
  ("p" mcl/turn-current-window-into-frame "pop current window into its own frame")
  ("q" hydra-window/body "quit"))

(defhydra hydra-window (:color amaranth :idle 0.2)
  "
_↑__↓_ resize vertically _←__→_ resize horizontally _<return>_ maximize _DEL_ delete
_<C-right>_ split right _<C-down>_ split down _o_ focus other window _SPC_ rotate windows _<C-SPC>_ rotate layout
_r_ save window configuration _z_ undo modification _Z_ redo modification _f_ frame operations... _q_ quit
"
  ("<right>" enlarge-window-horizontally nil) ("l" enlarge-window-horizontally nil)
  ("<left>" shrink-window-horizontally nil) ("h" shrink-window-horizontally nil)
  ("<down>" enlarge-window nil) ("j" enlarge-window nil)
  ("<up>" shrink-window nil) ("k" shrink-window nil)

  ("<return>" delete-other-windows nil :color blue)
  ("<deletechar>" delete-window nil :color blue) ("DEL" delete-window nil :color blue)
  ("<C-right>" mcl/hsplit-last-buffer nil :color blue) ("C-l" mcl/hsplit-last-buffer nil :color blue)
  ("<C-down>" mcl/vsplit-last-buffer nil :color blue) ("C-j" mcl/vsplit-last-buffer nil :color blue)
  ("o" other-window nil :color blue)
  
  ("r" window-configuration-to-register nil :exit t)
  ("z" winner-undo nil) ("Z" winner-redo nil)
  ("f" hydra-frame/body nil :exit t)
  ("SPC" rotate-window nil) ("<C-SPC>" rotate-layout nil)
  ("q" nil nil)
  ("ESC" nil nil))
(bind-key "C-x w" 'hydra-window/body)

;; End of Windows/Frames


(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1) :color pink :hint nil :post (deactivate-mark))
  "
_←__h_ _↓__j_ _↑__k_ _→__l_
_SPC_ Insert spaces   <C-_SPC_> Replace with spaces   Replace with _t_ext
_n_umber-lines        _e_xchange-point                _r_eset-region-mark (toggle)
_x_ cut/kill          _c_ copy                        _v_ paste/yank
"
  ("<up>" rectangle-previous-line)      ("k" rectangle-previous-line)
  ("<down>" rectangle-next-line)        ("j" rectangle-next-line)
  ("<left>" rectangle-backward-char)    ("h" rectangle-backward-char)
  ("<right>" rectangle-forward-char)    ("l" rectangle-forward-char)
  ("<S-delete>" kill-rectangle)         ("x" kill-rectangle) ;; C-x r k
  ("<C-insert>" copy-rectangle-as-kill) ("c" copy-rectangle-as-kill) ;; C-x r M-w
  ("<S-insert>" yank-rectangle)         ("v" yank-rectangle) ;; C-x r y
  ("SPC" open-rectangle :exit t)          ;; C-x r o
  ("<C-SPC>" clear-rectangle :exit t)     ;; C-x r c
  ("t" string-rectangle :exit t)          ;; C-x r t
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("n" rectangle-number-lines :exit t)    ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("q" nil "quit"))
(bind-key "C-x SPC" 'hydra-rectangle/body)

(defun mcl/newline-dwim ()
  "Eletric newline -- two line breaks if cursor is followed by closing parens.
From: github.com/magnars/.emacs.d/blob/5ff65739ebda23cfeffa6f70a3c7ecf49b6154ae/defuns/editing-defuns.el#L24"
  (interactive)
  (paredit-newline)
  (when (looking-at-p "[])}>]")
    (save-excursion
      (paredit-newline)
      (indent-for-tab-command)))
  (indent-for-tab-command))

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "C-M-x") nil)
          (define-key paredit-mode-map (kbd "M-:")   nil)
          (define-key paredit-mode-map (kbd "M-h")   nil)
          (define-key paredit-mode-map (kbd "M-j")   nil)
          (define-key paredit-mode-map (kbd "M-k")   nil)
          (define-key paredit-mode-map (kbd "M-l")   nil)
          (define-key paredit-mode-map (kbd "M-J")   nil)

          (define-key paredit-mode-map (kbd "<M-up>") nil)
          (define-key paredit-mode-map (kbd "<M-down>") nil)
          (define-key paredit-mode-map (kbd "<C-left>") nil)
          (define-key paredit-mode-map (kbd "<C-right>") nil)

          (define-key paredit-mode-map (kbd "M-?")   nil)
          (define-key paredit-mode-map (kbd "M-q")   nil)

          (define-key paredit-mode-map (kbd "C-d") nil)

          (define-key paredit-mode-map (kbd "C-M-k") nil)

          (define-key paredit-mode-map (kbd "C-M-`") 'cljr-raise-sexp)
          (define-key paredit-mode-map (kbd "C-M-1") 'paredit-join-sexps)
          (define-key paredit-mode-map (kbd "C-M-2") 'paredit-split-sexp)
          (define-key paredit-mode-map (kbd "C-M-3") 'paredit-splice-sexp)
          (define-key paredit-mode-map (kbd "C-M-4") 'paredit-splice-sexp-killing-backward)
          (define-key paredit-mode-map (kbd "C-M-5") 'paredit-splice-sexp-killing-forward)

          (define-key paredit-mode-map (kbd "<C-M-right>") nil)
          (define-key paredit-mode-map (kbd "<C-M-left>") nil)

          (define-key paredit-mode-map (kbd "<C-M-up>")    'paredit-backward-up)
          (define-key paredit-mode-map (kbd "<C-M-down>")  'paredit-forward-down)
          (define-key paredit-mode-map (kbd "<M-S-left>")  'paredit-backward)
          (define-key paredit-mode-map (kbd "<M-S-right>") 'paredit-forward)

          (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
          (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)

          (define-key paredit-mode-map (kbd "<M-delete>") 'paredit-forward-kill-word)
          (define-key paredit-mode-map (kbd "<C-delete>") 'paredit-forward-kill-word)
          (define-key paredit-mode-map (kbd "<M-backspace>") 'paredit-backward-kill-word)
          (define-key paredit-mode-map (kbd "<C-backspace>") 'paredit-backward-kill-word)
          (define-key paredit-mode-map (kbd "<C-M-backspace>") 'backward-kill-sexp)

          (define-key paredit-mode-map (kbd "RET") 'mcl/newline-dwim)
))

;; Google Cloud in Tramp mode
;; https://github.com/dustinfreeman/dustinfreeman.emacs.d/blob/master/init.el#L50

(defun isearch-delete-something ()
  "In isearch, delete non-matching text or the last character."
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))
(define-key isearch-mode-map (kbd "<backspace>") #'isearch-delete-something)

(define-key isearch-mode-map (kbd "<down>")   'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<up>")   'isearch-repeat-backward)

;; Put the cursor in an intelligent place when searching
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (and isearch-forward isearch-other-end
       (not mark-active)
       (not isearch-mode-end-hook-quit)
       (goto-char isearch-other-end)))

"Add extra actions for when M-o is pressed in ivy-switch-buffer (C-x b)."
(ivy-add-actions
 'ivy-switch-buffer
 '(("f"
    (lambda (x)
      (let ((filename (if (file-name-absolute-p x)
                          x
                        (buffer-file-name (get-buffer x)))))
        (if (not filename)
            (message "Buffer has no associated file.")
          (kill-new filename)
          (message filename))))
    "copy full filename")
   ("d"
    (lambda (x)
      (let* ((filename (if (file-name-absolute-p x)
                           x
                         (buffer-file-name (get-buffer x))))
             (dirname (when filename (file-name-directory filename))))
        (if (not filename)
            (message "Buffer has no associated file.")
          (kill-new dirname)
          (message dirname))))
    "copy dirname")
   ("b"
    (lambda (x)
      (let* ((filename (if (file-name-absolute-p x)
                           x
                         (buffer-file-name (get-buffer x))))
             (dirname (when filename (file-name-directory filename))))
        (if (not filename)
            (message "Buffer has no associated file.")
          (browse-url-xdg-open dirname))))
    "browse directory in file manager")))

(define-key minibuffer-local-map (kbd "<return>") 'exit-minibuffer)

(use-package smartparens
  :defer nil
  :diminish "{}"
  :bind (("C-M-1" . #'multiple-cursors-hydra/body)
         ("C-)" . #'sp-slurp-hybrid-sexp)
         ("C-M-S-c" . #'sp-convolute-sexp))
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-strict-mode 1)))

(use-package smart-jump
  :defer nil
  :config (smart-jump-setup-default-registers))

(use-package format-all
  :bind*
  ("C-M-<tab>" . format-all-buffer))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package eldoc
  :diminish nil)

;; Clojure

(use-package clojure-mode-extra-font-locking
  :after (clojure-mode))

(use-package clojure-snippets
  :defer t)

(use-package flycheck-joker
  :init (require 'flycheck-joker))

(use-package flycheck-clj-kondo
  :init
  (progn (require 'flycheck-clj-kondo)
         (dolist (checkers '((clj-kondo-clj . clojure-joker)
                             (clj-kondo-cljs . clojurescript-joker)
                             (clj-kondo-cljc . clojure-joker)))
           (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))))

(use-package cider                      ; TODO: cleanup
  :diminish ""
  :bind ("C-c r" . cider-ns-refresh)
  :custom
  ;; (cider-jdk-src-paths '("~/.java/openjv-8-src/"
  ;;                        "~/src/opensource/clojure/src/jvm"))
  (cider-lein-command "~/bin/lein")
  (cider-ns-save-files-on-refresh t)
  (cider-prompt-for-symbol nil t)
  (cider-repl-display-help-banner nil)
  (cider-repl-pretty-print-width 250 t)
  (cider-repl-use-pretty-printing t)
  (cider-save-file-on-load t)

  :hook
  ((cider-repl-mode . enable-paredit-mode)
   (cider-mode . eldoc-mode)))

(use-package clojure-mode
  :bind (("C-c ;" . clojure-toggle-keyword-string))
  :custom
  (clojure-align-binding-forms
   '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs"))
  :config (progn ;; Clojure indentation
            (put 'definterceptor 'clojure-doc-string-elt 2)
            (put 't/defschema 'clojure-doc-string-elt 2)))

(use-package clj-refactor
  :diminish ""
  :after (clojure cider)
  :init (add-hook 'clojure-mode-hook (lambda ()
                                       (clj-refactor-mode 1)
                                       (yas-minor-mode 1) ; for adding require/use/import statements
                                       (cljr-add-keybindings-with-prefix "C-c C-x"))))

;; Go

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(use-package lsp-ui
  :requires (lsp-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-border "#5d737a")
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package company-lsp
  :requires (company lsp-mode)
  :commands company-lsp)

;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-mode
:defer t
:mode ("\\.go\\'" . go-mode)
:init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
:bind (("M-," . compile)
       ("M-." . godef-jump)))

;; (use-package go-errcheck)

;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '(company-go))
;;             (company-mode)
;;             (if (not (string-match "go" compile-command))
;;                 (set (make-local-variable 'compile-command)
;;                      "go build -v && go test -v && go vet"))
;;             (flycheck-mode)))

;; Rust

(use-package rustic
  :mode ("\\.rs\\'" . 'rustic-mode))
(use-package toml-mode)

(use-package racer
  :requires (company)
  :init (progn
          (add-hook 'racer-mode-hook #'company-mode)
          (add-hook 'racer-mode-hook #'eldoc-mode)))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package flycheck-rust
  :init (progn (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
               (add-hook 'rust-mode-hook 'flycheck-mode)))


(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode))
  :config  
  (require 'nix-drv-mode)
  (add-to-list 'auto-mode-alist '("\\.drv\\'" . nix-drv-mode)))

(use-package feature-mode
  :mode (("\.feature$" . feature-mode)))

(use-package org)

;; Python

(use-package python
  :defer 10
  :hook python-mode-hook
  :custom
  (python-shell-interpreter "ipython3"))

(use-package ein
  :requires (company)
  :config
  ; (advice-add 'request--netscape-cookie-parse :around #'fix-request-netscape-cookie-parse)
  (setq ein:worksheet-enable-undo 'yes)
  (setq ein:truncate-long-cell-output 40)
  (setq ein:connect-mode-hook 'ein:use-company-backend)
  (progn
    (setq ein:default-url-or-port "https://shell.drakirus.com")))

(use-package elpy
  :requires (company)
  :after (company python)
  :init (elpy-enable)
  :config
  ;; (when (executable-find "ipython")
  ;;   (setq python-shell-interpreter "ipython"
  ;;         python-shell-interpreter-args "-i --simple-prompt"))

  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules))

(use-package py-autopep8
  :after elpy
  :hook (elpy-mode . py-autopep8-enable-on-save))

(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'."
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

;; End of Python

;; Javascript

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)))

(use-package json-mode
  :defer nil)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;; End of Javascript

(use-package string-inflection
  :config
  (defhydra hydra-string-inflection (:color teal :idle 0.5 :columns 1)
    ("c" string-inflection-lower-camelcase "camelCase")
    ("p" string-inflection-camelcase "PascalCase")
    ("-" string-inflection-kebab-case "kebab-case")
    ("_" string-inflection-underscore "snake_case")
    ("s" string-inflection-upcase "SCREAMING_SNAKE_CASE")
    ("q" nil "quit"))
  (bind-key (kbd "C-;") 'hydra-string-inflection/body))

(use-package imenu-list
  :bind (("C-'" . imenu-list-smart-toggle)))

(defhydra hydra-goto (:color blue :columns 3)
  ("<tab>" move-to-column "column")
  ("b" counsel-bookmark "bookmark")
  ("c" goto-char "char")
  ("g" goto-line "line") ("M-g" goto-line "")
  ("s" counsel-imenu "symbol definitions")
  ("m" counsel-mark-ring "mark")
  ("n" next-error "next-error") ("M-n" next-error "")
  ("p" previous-error "previous-error") ("M-p" previous-error "previous-error")
  ("q" nil "quit"))
(bind-key "M-g" 'hydra-goto/body)

(use-package dired-sidebar
  :bind ("<f5>" . dired-sidebar-toggle-sidebar)
  :commands (dired-sidebar-toggle-sidebar))
(require 'dired-x)

(bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(bind-key "C-+" 'hs-toggle-hiding)

;; Considering:
;; Two-paned dired: http://pragmaticemacs.com/emacs/double-dired-with-sunrise-commander/

;; Good dark color themes: base16-tomorrow-dark, base16-oceanicnext-dark, zenburn, apropospriate-dark, gruvbox-dark-medium, tangotango
;; Good light color themes: flatui, solarized, faff

(defadvice load-theme (before theme-dont-propagate activate)
  "Ensure only one theme is active at a time."
  (mapc #'disable-theme custom-enabled-themes))

(use-package doom-themes
  :init
  (load-theme 'doom-tomorrow-night t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(use-package beacon
  :diminish beacon-mode
  :init
  (beacon-mode 1)
  :custom
  (beacon-blink-when-point-moves-horizontally 10)
  (beacon-blink-when-point-moves-vertically 1))

(use-package all-the-icons
  :defer nil
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts))
  :init (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-ivy
  :requires (ivy)
  :defer nil
  :hook ((after-init . all-the-icons-ivy-setup)))

(use-package all-the-icons-dired
  :requires (all-the-icons)
  :hook ((dired-mode . all-the-icons-dired-mode)))
