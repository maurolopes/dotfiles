;; Startup time
;; (emacs-init-time) ; 0.8 s
;; https://github.com/CSRaghunandan/.emacs.d/blob/master/init.el#L7

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defun reset-gc-threshold ()
  "Reset threshold of garbage collector to its original value, and invoke garbage colletion now."
  (setq gc-cons-threshold gc-cons-threshold--orig
        file-name-handler-alist file-name-handler-alist-old)
  (garbage-collect))

(add-hook 'emacs-startup-hook #'reset-gc-threshold)

;; End of Startup time

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package diminish
  :config (diminish 'subword-mode))

(use-package auto-package-update
  :config (auto-package-update-maybe))

(use-package default-text-scale)
(use-package hydra)
(use-package ag)

(use-package ws-butler ; Cleanup whitespace
  :defer nil
  :config (ws-butler-global-mode t))

(use-package no-littering               ; Keep .emacs.d clean
  :defer nil
  :config
  (require 'no-littering)
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

(use-package recentf
  :after (no-littering)
  :config (progn
            (add-to-list 'recentf-exclude no-littering-etc-directory)
            (add-to-list 'recentf-exclude no-littering-var-directory)
            (recentf-mode 1)))

(use-package keyfreq
  :init (progn (add-hook 'after-init-hook 'keyfreq-mode t)
               (add-hook 'after-init-hook 'keyfreq-autosave-mode t)))

(use-package git-gutter-fringe
  :config (global-git-gutter-mode t))

(use-package volatile-highlights
  :diminish nil
  :config (volatile-highlights-mode t))

(use-package which-key
  :diminish nil
  :config (which-key-mode +1))

(use-package shackle ; tame window popups
  :defer nil
  :config (progn
            (setq shackle-rules
                  '((compilation-mode              :select nil                                               )
                    ("*undo-tree*"                                                    :size 0.25 :align right)
                    ("*eshell*"                    :select t                          :other t               )
                    ("*Shell Command Output*"      :select nil                                               )
                    ("\\*Async Shell.*\\*" :regexp t :ignore t                                                 )
                    (occur-mode                    :select nil                                   :align t    )
                    ("*Help*"                      :select t   :inhibit-window-quit t :other t               )
                    ("*Completions*"                                                  :size 0.3  :align t    )
                    ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
                    ("\\*[Wo]*Man.*\\*"    :regexp t :select t   :inhibit-window-quit t :other t               )
                    ("\\*poporg.*\\*"      :regexp t :select t                          :other t               )
                    ("\\`\\*helm.*?\\*\\'"   :regexp t                                    :size 0.3  :align t    )
                    ("*Calendar*"                  :select t                          :size 0.3  :align below)
                    ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
                    (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
                    (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
                    (:select t)))
            (shackle-mode +1)))

(use-package undo-tree
  :diminish nil
  :bind ("<S-undo>" . undo-tree-redo)
  :config (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  :init (global-undo-tree-mode))

(use-package centered-cursor-mode
  :defer nil
  :diminish (centered-cursor-mode . " ⊝")
  :config
  (require 'centered-cursor-mode)
  (global-centered-cursor-mode +1))

(use-package ivy
  ;;TODO :after (centered-cursor-mode) ; doesn't work to solve the problem of <next> (PgDn) in Ivy minibuffer
  :diminish nil
  :init
  (use-package historian
    :init (historian-mode +1)
    :config
    (use-package ivy-historian
      :config (ivy-historian-mode t)))
  (ivy-mode +1)
  :config
  (define-key ivy-minibuffer-map (kbd "<C-down>") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "<C-up>") 'ivy-previous-history-element))

(use-package ivy-hydra
  :after (ivy hydra)
  :config (require 'hydra))

(use-package avy
  :bind ("C-l" . avy-goto-subword-1))

(use-package company
  :defer nil
  :diminish nil
  :config (global-company-mode))

(use-package column-enforce-mode
  :diminish nil
  :hook ((prog-mode . column-enforce-mode))
  :config (face-spec-set column-enforce-face '((t (:background "red")))))

(use-package restclient
  :mode "\\.rest$")

(use-package company-restclient
  :after (company restclient)
  :hook ((restclient-mode . (lambda () (add-to-list 'company-backends 'company-restclient)))))

(use-package mwim
  :bind (("<home>" . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line)))

(use-package amx ; better M-x interface -- integrates with Ivy
  :diminish "amx"
  :config
  (amx-mode t))

(use-package counsel
  :defer nil
  :diminish nil
  :config (counsel-mode))

(use-package projectile
  :diminish nil
  :config (projectile-mode +1)
  :bind-keymap ("M-p" . projectile-command-map))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package swiper
  :bind ("C-f" . swiper))

(use-package vlf ; view large files
  :config (require 'vlf-setup))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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
  :bind ("C-x m" . #'multiple-cursors-hydra/body))

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

(defun mcl/kill-this-buffer ()
  ;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(bind-key "C-x k" 'mcl/kill-this-buffer)


(define-key undo-tree-map (kbd "C-/") nil)
(bind-key "C-/" 'hippie-expand)
(bind-key "C-." 'repeat)
(bind-key "C-z" 'undo-tree-undo)
(bind-key "C-S-z" 'undo-tree-redo)
(bind-key "C-^" (lambda () (interactive) (delete-indentation t))) ; join-line top-down

(bind-key "M-o" 'other-window)
(bind-key "M-f" 'counsel-ag)
(bind-key "C-x C-r" 'counsel-recentf)
(bind-key "C-x C-l" 'counsel-locate)

(defalias 'yes-or-no-p 'y-or-n-p)

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
  "
%(mcl/toggle-str column-number-mode) _c_olumn-number            %(mcl/toggle-str display-battery-mode) _b_attery-mode
%(mcl/toggle-str debug-on-error) d_e_bug-on-error           %(mcl/toggle-str debug-on-quit) debug-on-q_u_it
%(mcl/toggle-str global-git-gutter-mode) global-_G_it-gutter-mode
%(mcl/toggle-str auto-fill-function) auto-_f_ill                %(mcl/toggle-str whitespace-mode) _w_hitespace
%(mcl/toggle-str hl-line-mode) _h_ighlight-line           %(mcl/toggle-str global-hl-line-mode) global-_H_ighlight-line
%(mcl/toggle-str truncate-lines) _t_runcate-lines           %(mcl/toggle-str buffer-read-only) _r_ead-only
%(mcl/toggle-str visual-line-mode) _v_isual-line              %(mcl/toggle-str global-visual-line-mode) global-_V_isual-line
%(mcl/toggle-str display-line-numbers-mode) display-line-_n_umbers     %(mcl/toggle-str global-display-line-numbers-mode) global-display-line-_N_umbers
"
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
  ("g" git-gutter-mode nil) ;; does not work
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

(defhydra hydra-window (:color amaranth :columns 2 :idle 0.5)
  "
_↑__↓_ resize window vertically
_←__→_ resize window horizontally
"
  ("<right>" enlarge-window-horizontally nil) ("l" enlarge-window-horizontally nil)
  ("<left>" shrink-window-horizontally nil) ("h" shrink-window-horizontally nil)
  ("<down>" enlarge-window nil) ("j" enlarge-window nil)
  ("<up>" shrink-window nil) ("k" shrink-window nil)
  ("<return>" delete-other-windows "maximize this window" :color blue)
  ("<delete>" delete-window "close this window" :color blue)
  ("<C-right>" mcl/hsplit-last-buffer "split right" :color blue) ("C-l" mcl/hsplit-last-buffer :color blue)
  ("<C-down>" mcl/vsplit-last-buffer "split down" :color blue) ("C-j" mcl/vsplit-last-buffer :color blue)
  ("o" other-window "move cursor to other window" :color blue)
  ("f" make-frame "new frame" :color blue)
  ("d" delete-frame "delete frame" :color blue)
  ("p" mcl/turn-current-window-into-frame "pop window into new frame" :color blue)
  ("r" window-configuration-to-register "Save window configuration" :exit t)
  ("z" winner-undo "Undo window modification")
  ("Z" winner-redo "Redo window modification")
  ("q" nil "quit"))
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

(use-package eval-sexp-fu
  :commands turn-on-eval-sexp-fu-flash-mode)

(use-package smartparens
  :defer nil
  :diminish "{}"
  :config
  (progn
    (require 'smartparens-config)
    (bind-key "C-;" 'sp-kill-sexp)
    (smartparens-global-strict-mode 1)))

(use-package smart-jump
  :after (paredit)
  :config (smart-jump-setup-default-registers))

;; (use-package prettify-symbols
;;   :init (global-prettify-symbols-mode t)
;;   :config
;;   (progn
;;     (add-hook 'emacs-lisp-mode-hook (lambda ()
;;                                       (push '(">=" . ?≥) prettify-symbols-alist)
;;                                       (push '("<=" . ?≤) prettify-symbols-alist)))
;;     (let ((transforms '(("->" . ?→)
;;                         ("->>" . ?) ;⇉)
;;                         ("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
;;                         ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
;;                                        (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
;;                                        (Bc . Bl) ?- (Br . Br) ?>))
;;                         (">=" . ?≥)
;;                         ("<=" . ?≤)
;;                         ("not=" . ?≠)))))
;;     (add-hook 'clojure-mode (lambda ()
;;                               (setq clojure--prettify-symbols-alist
;;                                     (append transforms clojure--prettify-symbols-alist))))))

;; (eval-after-load 'clojure-mode '(define-clojure-indent (given :defn)))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package eldoc
  :diminish nil)

;; Clojure

(use-package clojure-mode-extra-font-locking
  :after (clojure-mode))

(use-package flycheck-joker
  :init (require 'flycheck-joker))

(use-package cider ; TODO: cleanup
  :diminish ""
  ;; :bind ("C-c k" . cider-ns-refresh)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-pretty-print-width 250)
  (cider-jdk-src-paths '("~/.java/openjv-8-src/"
                         "~/src/opensource/clojure/src/jvm"))
  :hook
  ((cider-repl-mode . enable-paredit-mode)
   (cider-mode . eldoc-mode)))

(use-package cider-eval-sexp-fu
  :after (cider eval-sexp-fu)
  :commands cider-esf--bounds-of-last-sexp)

(use-package clojure-mode
  :bind (("C-c ;" . clojure-toggle-keyword-string))
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

;; Rust

(use-package rustic)
(use-package toml-mode)

(use-package racer
  :init (progn
          (add-hook 'racer-mode-hook #'company-mode)
          (add-hook 'racer-mode-hook 'eldoc-mode)))

(use-package cargo
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :init (progn (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
               (add-hook 'rust-mode-hook 'flycheck-mode)))

;; End of Rust

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode))
  :config  
  (require 'nix-drv-mode)
  (add-to-list 'auto-mode-alist '("\\.drv\\'" . nix-drv-mode)))

(use-package feature-mode
  :mode (("\.feature$" . feature-mode)))

;; Python

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (add-hook 'elpy-mode-hook 'flycheck-mode))


(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'."
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

;; End of Python

;; Javascript

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)))

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

;; Considering:
;; Two-paned dired: http://pragmaticemacs.com/emacs/double-dired-with-sunrise-commander/

;; Good dark color themes: base16-tomorrow-dark, base16-oceanicnext-dark, zenburn, apropospriate-dark
;; Good light color themes: flatui, solarized, faff

(defadvice load-theme (before theme-dont-propagate activate)
  "Ensure only one theme is active at a time."
  (mapc #'disable-theme custom-enabled-themes))

(use-package eziam-theme
  :init (load-theme 'eziam-light))
