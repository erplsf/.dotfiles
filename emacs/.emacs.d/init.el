;;; init.el -*- lexical-binding: t; -*-

;; set up straight.el

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

;; install use-package and configure it to use straight

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; some community features (cleanup and stuff

;; (require 'straight-x)

;; make use-package always use straight

(setq straight-use-package-by-default t)

;; set up theme

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; set up font

(set-face-attribute 'default nil
                    :family "Input Mono"
                    :weight 'light
                    :height 140)
;; :height 200)

;; set default for symbols

(set-fontset-font "fontset-default" nil "MesloLGS NF")

;; important stuff

(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0)                                ; Disable the tooltips
  (add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Start maximized, whatever that could mean
  (set-frame-parameter (selected-frame) 'alpha 90) ; enable true transparency when compositor (Picom) is running
  (add-to-list 'default-frame-alist '(alpha . 90))) ; same

(fringe-mode 1)                                 ; TODO: maybe reenable them sometime?
(menu-bar-mode 0)                                ; Disable the menu bar
(setq inhibit-splash-screen t)                   ; Inhibit the starting splash screen

(setq-default fill-column 120)
(setq-default indent-tabs-mode nil) ; tabs are evil, use spaces
(setq-default tab-width 2)
(setq-default js-indent-level 2)

;; increase gc/related configs for lsp

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))
(setq read-process-output-max (* 1024 1024))

;; edit this file

(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c I") 'find-config)

;; set up other packages
;; org

;; don't show async-shell-command on execution
(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

(defun am/start-pause-afk-clock ()
  "TODO: add documentation / intent ;)
   TODO: change to use ids?"
  (interactive)
  (org-with-point-at (org-find-exact-heading-in-directory "Non-working time" (car org-agenda-files))
    (if (org-clock-is-active)
        (prog1
          (org-clock-out)
          (async-shell-command "polybar-msg hook org 1"))
      (prog1
        (org-clock-in)
        (async-shell-command "polybar-msg hook org 2")))))

(use-package org
  :bind
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  ("C-c a" . org-agenda)
  ("C-c k t" . am/start-pause-afk-clock)
  :config
  (require 'ox-md)
  (require 'org-habit)
  (require 'org-refile)                 ; requried to fix 'org-get-outline-path': https://github.com/hlissner/doom-emacs/issues/2757
  (add-to-list 'org-modules 'org-habit t)
  (org-clock-persistence-insinuate)
  :custom
  (org-babel-load-languages '((python . t)
                              (J . t)))
  (org-babel-J-command "j9 -c")
  (org-confirm-babel-evaluate nil) ; TODO: unsafe!
  (org-directory "~/org")
  (org-default-notes-file "~/org/inbox.org")
  (org-agenda-files '("~/org/gtd"))
  (org-archive-location "~/org/gtd/archive/archive.org::* From %s")
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
  (org-capture-templates '(("t" "todo" entry (file "~/org/gtd/refile.org") "* TODO %?")))
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")))
  (org-return-follows-link t)
  ;; agenda configs
  (org-deadline-warning-days 7)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-on-weekday nil)
  (org-clock-persist 'history)
  (org-habit-show-habits-only-for-today nil)
  (org-agenda-show-future-repeats nil)
  (org-agenda-custom-commands
   '(("a" "Super custom view"
      ((agenda "" ((org-agenda-span 'week)
                   (org-super-agenda-groups
                    '((:time-grid t)))))
       (alltodo "" ((org-agenda-overriding-header "")
                    (org-agenda-block-separator "-")
                    (org-super-agenda-groups
                     '((:name "Next"
                              :tag "next")
                       ;; (:name "Active films"
                       ;;        :todo ("ACTIVE" "TOWATCH"))
                       (:discard (:anything t)))))))))))

;; advanced org

(use-package org-edna
  :config
  (org-edna-mode))

;; rainbow-delimiters

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; company TODO: review (because I just copied it over from angrybacon)

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-backends '(company-capf))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-other-buffers nil)
  (company-global-modes '(not help-mode message-mode))
  (company-idle-delay .0)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-offset-display nil)
  (company-tooltip-width-grow-only t)
  :config
  (company-tng-mode))

(use-package company-box
  :hook
  (company-mode . company-box-mode)
  :custom
  (company-box-max-candidates 50)
  (company-box-scrollbar nil)
  (company-box-show-single-candidate 'always))

;; projectile

(use-package projectile
  :hook
  (after-init . projectile-global-mode)
  (projectile-after-switch-project-hook . projectile-invalidate-cache)
  :bind
  ("C-c p" . projectile-command-map)
  :init
  (setq-default
   projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory)
   projectile-known-projects-file (expand-file-name ".projectile-bookmarks" user-emacs-directory))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'default))

(use-package projectile-ripgrep)

(use-package persp-projectile
  :bind
  ("C-c p p" . projectile-persp-switch-project))

;; treemacs

(use-package treemacs)

(use-package treemacs-projectile)

;; yaml

(use-package yaml-mode :mode "\\.yml\\'" "\\.yaml\\'")

;; magit + forge glory

(use-package magit
  :bind (:map magit-file-section-map
              ("<C-return>" . magit-diff-visit-file-other-window))
  :init
  (setq magit-pull-or-fetch t))

(use-package forge
  :after magit)

(setq auth-sources '("~/.authinfo"))

;; (add-to-list 'forge-alist ;; broke
;;              '("github-leanix"
;;                "api.github.com"
;;                "github.com"
;;                forge-github-repository))

;; eyebrowse
(setq eyebrowse-keymap-prefix (kbd "C-c e"))

(use-package eyebrowse
  :disabled
  :hook
  (after-init . eyebrowse-mode)
  :custom
  (eyebrowse-new-workspace t))

;; desktop

(use-package desktop
  :disabled
  :hook
  (after-init . desktop-read)
  (after-init . desktop-save-mode))

;; aggressive-indent

(use-package aggressive-indent
  :hook
  (c++-mode . aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'java-mode)
  :custom
  (aggressive-indent-comments-too t)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; which-key

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; smex

(use-package smex)

;; selectrum and friends (filtering and selection and stuff)

(use-package selectrum
  :config (selectrum-mode +1))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))
  
(use-package ctrlf
  :config
  (ctrlf-mode +1))

;; flycheck

(use-package flycheck
  :hook
  (ruby-mode . flycheck-mode)
  (rust-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-display-errors-delay .3)
  :init
  (global-flycheck-mode))

;; rust

(use-package rust-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :config
  (progn
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
	  (add-hook 'toml-mode-hook 'cargo-minor-mode)))

;; go

(use-package go-mode)

(defun am/go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook `am/go-mode-hook)

;; lsp (and it's glory suite)

;; reconfigure from the scratch, need to investigate more

(setq lsp-keymap-prefix (kbd "C-c l")
      lsp-auto-guess-root t
      lsp-completion-provider :capf
      lsp-idle-delay 1
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      ;;      
      lsp-solargraph-use-bundler t
      lsp-haskell-process-path-hie "hie-wrapper"
      lsp-csharp-server-path "/usr/bin/omnisharp"
      ) 

(use-package lsp-mode
  :hook
  ((ruby-mode . lsp-deferred)
   (haskell-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (terraform-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (java-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (csharp-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  ;; :custom
  ;; lsp-rust-build-on-save t
  )

(use-package yasnippet :config (yas-global-mode))

(use-package lsp-haskell
  :after lsp-mode)

(use-package lsp-java
  :after lsp-mode)

(use-package lsp-ui
  :custom
  lsp-ui-doc-position 'at-point)

(use-package lsp-treemacs)

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("/bin/terraform-ls" "serve"))
;;                   :major-modes '(terraform-mode)
;;                   :server-id 'terraform-ls))

;; (add-hook 'terraform-mode-hook #'lsp-mode)

;; typescript/javascript (needs `npm install -g typescript-language-server`)
;; a hack too

;; (add-to-list 'exec-path (expand-file-name "~/.nodenv/shims"))

;; manage versions better

(setq
 backup-by-copying t
 backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
 auto-save-file-name-transforms `((".*" "~/.emacs.d/saves" t))
 delete-old-versions t
 version-control t
 kept-new-versions 10
 kept-old-versions 5)

;; hledger

(defun am/ledger-insert-effective-date (&optional date)
  "Insert effective date `DATE' to the transaction or posting.

If `DATE' is nil, prompt the user a date.

Replace the current effective date if there's one in the same
line.

With a prefix argument, remove the effective date."
  (interactive)
  (if (and (listp current-prefix-arg)
           (= 4 (prefix-numeric-value current-prefix-arg)))
      (ledger-remove-effective-date)
    (let* ((context (car (ledger-context-at-point)))
           (date-string (or date (ledger-read-date "Effective date: "))))
      (save-restriction
        (narrow-to-region (point-at-bol) (point-at-eol))
        (cond
         ((eq 'xact context)
          (beginning-of-line)
          (re-search-forward ledger-iso-date-regexp)
          (when (= (char-after) ?=)
            (ledger-remove-effective-date))
          (insert "=" date-string))
         ((eq 'acct-transaction context)
          (end-of-line)
          (ledger-remove-effective-date)
          (insert "  ; cleared on date:" date-string ))))))) 

(use-package ledger-mode
  :mode "\\.journal\\'"
  :bind (:map ledger-mode-map ("C-c C-t" . am/ledger-insert-effective-date))
  :config
  (setq ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-binary-path "hledger")
  :custom
  (ledger-post-account-alignment-column 2 "Obey the common standard")
  (ledger-post-amount-alignment-column 0 "Don't align amounts for now"))

;; windmove

;; (use-package windmove
;;   :bind
;;   (("C-M-<left>". windmove-left)
;;    ("C-M-<right>". windmove-right)
;;    ("C-M-<up>". windmove-up)
;;    ("C-M-<down>". windmove-down)))

;; org-super-agenda

(use-package org-super-agenda
  :hook
  (after-init . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '((:name "Next" :tag "next" :todo t)
          (:habit t))))

;; Dockerfile mode

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; terraform mode

(use-package terraform-mode
   :config
   (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; haskell-mode

(use-package haskell-mode
  :init
  (setq haskell-process-type 'stack-ghci)
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

;; ace-window

(global-set-key (kbd "M-o") 'ace-window)
(use-package ace-window)

;; traad-rename (python)

;; (use-package traad)

(use-package markdown-mode)

;; update packages automagically

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; (use-package rbenv
;;  :hook
;;  (after-init . global-rbenv-mode))

;; ag frontend

(use-package ag)

;; hydra

(use-package hydra)

;; smerge hydra

(use-package smerge-mode
  :after hydra
  :config
  (defhydra am/unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (am/unpackaged/smerge-hydra/body)))))


;; minions

(use-package minions
  :config (minions-mode 1))

;; moody

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  :custom
  (moody-mode-line-height 35))

;; pdf-tools
(use-package pdf-tools
  :config
  (pdf-tools-install))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; dap-mode (for delve -> golang)
(use-package dap-mode)
(require 'dap-go)

;; jsonnet-mode
(use-package jsonnet-mode)

;; json-mode
(use-package json-mode)

;; persistent-scratch
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;; lua-mode

(use-package lua-mode)

;; diff-hl

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; undo-tree

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; perspective

(use-package perspective
  :bind-keymap ("C-x x" . perspective-map)
  :bind (("C-x b" . persp-switch-to-buffer)
         ("C-x k" . persp-kill-buffer*))
  :init (setq-default persp-initial-frame-name "personal")
  :custom (persp-state-default-file (expand-file-name ".perspective-state" user-emacs-directory))
  :hook
  (kill-emacs . persp-state-save)
  (after-init . (lambda ()
                  (persp-state-load persp-state-default-file)))
  :config
  (persp-mode))


;; system-packages

(use-package system-packages)

(use-package use-package-ensure-system-package
  :ensure t)

;; magit-todos

(use-package magit-todos
  :config
  (magit-todos-mode)
  :ensure-system-package rg)

;; PKGBUILD

(use-package pkgbuild-mode)

;; ebuild

(use-package ebuild-mode)

;; origami (folding)

(use-package origami)

;; web-mode (for golang templates)

(use-package web-mode
  :init
  (setq web-mode-engines-alist '(("go" . "\\.tmpl\\'"))))

;; emms

(use-package emms
  :config
  (emms-all)
  (emms-default-players))


;; emacs-ccls

(use-package ccls
  :after projectile
  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

;; cmake

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-ide
  :after projectile
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command 
          (concat "cd " cmake-ide-build-dir " && cmake -GNinja .. && ninja"))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
  :init (cmake-ide-setup)
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

(use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style)
  (c-mode-common . google-make-newline-indent))

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package beacon
  :init
  (beacon-mode 1))

;; csharp

(use-package csharp-mode)

(global-set-key (kbd "C-c C-r") 'revert-buffer)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; modalka ;; TODO: disabled as setup is not finished yet

(use-package modalka
  :commands modalka-mode
  :bind ("C-c SPC" . modalka-mode)
  :config
  (modalka-define-kbd "W" "M-w")
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "g" "C-g")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "w" "C-w")
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "SPC" "C-SPC"))

;; (setq modalka-cursor-type 'hollow-box)

;; general TODO: maybe?

;; (use-package general)

;; evil?

;; dev stuff TODO: move out to separate packages

(use-package s)
(use-package dash)
(use-package package-lint)
(use-package flycheck-package)

;; end of dev stuff

(use-package winner
  :init
  (winner-mode))

;; chicken-scheme

(use-package geiser
  :custom
  (geiser-chicken-binary "chicken-csi"))
