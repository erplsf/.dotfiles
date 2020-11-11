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
(setq straight-use-package-by-default t)

(eval-when-compile
  (require 'use-package))

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

(set-face-attribute 'default nil :family "Input" :height 140)

;; important stuff

(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0)                                ; Disable the tooltips
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (set-frame-parameter (selected-frame) 'alpha 90) ; enable true transparency when compositor (Picom) is running
  (add-to-list 'default-frame-alist '(alpha . 90))) ; same

(fringe-mode 0)                                  ; Disable fringes
(menu-bar-mode 0)                                ; Disable the menu bar
(setq inhibit-splash-screen t)                   ; Inhibit the starting splash screen

(setq-default fill-column 120)
(setq-default indent-tabs-mode nil) ; tabs are evil
(setq-default tab-width 2)
(setq-default js-indent-level 2)

;; increase gc/related configs for lsp

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; edit this file

(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") 'find-config)

; set up other packages
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
  (org-agenda-show-future-repeats nil))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; company

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-require-match 'never)  
  (company-idle-delay 0.5)
  :config
  (global-company-mode 1))

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
  (projectile-enable-caching t))

;; yaml

(use-package yaml-mode :mode "\\.yml\\'" "\\.yaml\\'")

;; magit + forgeglory

(use-package magit)
(use-package forge
  :after magit)

;; eyebrowse
(setq eyebrowse-keymap-prefix (kbd "C-c e"))

(use-package eyebrowse
  :hook
  (after-init . eyebrowse-mode)
  :custom
  (eyebrowse-new-workspace t))

;; desktop

(use-package desktop
  :hook
  (after-init . desktop-read)
  (after-init . desktop-save-mode))

;; aggressive-indent

(use-package aggressive-indent
  :custom
  (aggressive-indent-comments-too t))

;; which-key

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; smex

(use-package smex)

;; ivy

(use-package ivy
  :hook
  (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume)))

;; counsel

(use-package counsel
  :after ivy
  :config (counsel-mode))

;; swiper

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

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

(use-package cargo :ensure t
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
      lsp-prefer-capf t
      lsp-idle-delay 0.500
      lsp-solargraph-use-bundler t
      lsp-haskell-process-path-hie "hie-wrapper")

(use-package lsp-mode
  :hook
  ((ruby-mode . lsp-deferred)
   (haskell-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (terraform-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-haskell
  :after lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

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

;; (use-package org-super-agenda
;;   :hook
;;   (after-init . org-super-agenda-mode)
;;   :config
;;   (setq org-super-agenda-groups
;;         '((:name "Next" :tag "next" :todo "NEXT")
;;           (:habit t))))

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

;; counsel-projectile

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

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
