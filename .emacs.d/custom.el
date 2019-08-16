(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-history-length 32)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(auto-package-update-delete-old-versions t)
 '(auto-package-update-hide-results t)
 '(avy-keys
   (quote
    (111 105 101 110 100 104 119 102 112 121 117 108 103 106 122 120 99 118 109 98 107 97 114 115 116)))
 '(avy-subword-extra-word-chars nil)
 '(blink-cursor-mode t)
 '(ccm-recenter-at-end-of-file t)
 '(ccm-vpos-init (quote (round (* 21 (window-text-height)) 34)))
 '(cider-jdk-src-paths
   (quote
    ("~/.java/openjv-8-src/" "~/src/opensource/clojure/src/jvm")) t)
 '(cider-lein-command "~/bin/lein")
 '(cider-ns-save-files-on-refresh t)
 '(cider-prompt-for-symbol nil t)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-pretty-print-width 250 t)
 '(cider-repl-use-pretty-printing t)
 '(cider-save-file-on-load t)
 '(clojure-align-binding-forms
   (quote
    ("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs")))
 '(clojure-defun-indents (quote (e2e fact)))
 '(column-enforce-column 128)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.15)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-require-match nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-minimum-width 15)
 '(compilation-ask-about-save nil)
 '(compilation-read-command nil)
 '(compilation-scroll-output (quote first-error))
 '(compilation-window-height 10)
 '(compile-command "make")
 '(counsel-ag-base-command "ag --nocolor --nogroup --hidden %s")
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" default)))
 '(delete-old-versions t)
 '(elpy-rpc-python-command "python3")
 '(enable-recursive-minibuffers t)
 '(eval-sexp-fu-flash-duration 0.5)
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "NIX_PATH")))
 '(expand-region-contract-fast-key "DEL")
 '(fci-rule-color "#383838")
 '(git-gutter-fr:side (quote right-fringe))
 '(global-font-lock-mode t)
 '(global-subword-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-height 20)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(kept-new-versions 6)
 '(load-prefer-newer t)
 '(maximum-scroll-margin 0.5)
 '(mc/always-run-for-all t)
 '(menu-bar-mode nil)
 '(nrepl-log-messages t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (imenu-anywhere rotate json-mode exec-path-from-shell ripgrep anti-zenburn-theme zenburn-theme amx ws-butler feature-mode nix-drv-mode typescript-mode string-inflection default-text-scale magit-todos eglot rustic clojure-mode-extra-font-locking js2-mode prettify-symbols-mode prettify-symbols prog-mode column-enforce-mode eyebrowse company-restclient restclient no-littering which-key volatile-highlights vlf use-package undo-tree toml-mode smex smartparens smart-jump shackle rainbow-mode rainbow-delimiters racer nix-mode mwim magit keyfreq ivy-hydra git-gutter-fringe flycheck-rust flycheck-joker eziam-theme expand-region diminish counsel-projectile clj-refactor cider-eval-sexp-fu centered-cursor-mode cargo avy auto-package-update ag)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(python-shell-interpreter "ipython3")
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-menu-items 20)
 '(recentf-max-saved-items 500)
 '(ring-bell-function (quote ignore))
 '(scroll-margin 99999)
 '(scroll-preserve-screen-position t)
 '(shackle-default-size 0.4)
 '(show-paren-delay 0.0)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(use-package-minimum-reported-time 0)
 '(use-package-verbose t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(version-control t)
 '(vlf-tune-enabled nil)
 '(wakatime-python-bin nil)
 '(which-key-show-transient-maps t)
 '(which-key-side-window-max-height 0.75)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
