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

(set-face-attribute 'default nil :family "Input" :height 135)

;; important stuff

(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0)                                ; Disable the tooltips
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (set-frame-parameter (selected-frame) 'alpha 90) ; enable true transparency when compositor (Picom) is running
  (add-to-list 'default-frame-alist '(alpha . 90))) ; same

(fringe-mode 0)                                 ; TODO: maybe reenable them sometime?
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

(defun am/start-pause-afk-clock ()
  "TODO: add documentation / intent ;)
   TODO: change to use ids?"
  (interactive)
  (org-with-point-at (org-find-exact-heading-in-directory "Non-working time" (car org-agenda-files))
    (if (org-clock-is-active)
        (org-clock-out)
      (org-clock-in))))

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
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :date today)))))
       (alltodo "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '((:name "Next"
                              :tag "next")
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

;; yaml

(use-package yaml-mode :mode "\\.yml\\'" "\\.yaml\\'")

;; magit + forge glory

(use-package magit
  :init
  (setq magit-pull-or-fetch t))

(use-package forge
  :after magit)

(setq auth-sources '("~/.authinfo"))

(add-to-list 'forge-alist
             '("github-leanix"
               "api.github.com"
               "github.com"
               forge-github-repository))

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
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'java-mode)
  :custom
  (aggressive-indent-comments-too t))

;; which-key

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; smex

(use-package smex)

;; selectrum and friends

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
      lsp-completion-provider :capf
      lsp-idle-delay 1
      lsp-solargraph-use-bundler t
      lsp-haskell-process-path-hie "hie-wrapper")

(use-package lsp-mode
  :hook
  ((ruby-mode . lsp-deferred)
   (haskell-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (terraform-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (java-mode . lsp-deferred)
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

;; (use-package dockerfile-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

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
(use-package pdf-tools)
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
  :bind (("C-x b" . persp-switch-to-buffer*)
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

(use-package system-packages
  :custom
  system-packages-package-manager 'yay)

(use-package use-package-ensure-system-package
  :ensure t)

;; magit-todos

(use-package magit-todos
  :config
  (magit-todos-mode)
  :ensure-system-package rg)
