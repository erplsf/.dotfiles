;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Andriy Mykhaylyk"
      user-mail-address "erp.lsf@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; TODO: add a method for checking and falling back if font doesn't exist
(setq doom-font (font-spec :family "Jetbrains Mono" :size 18))

;; (setq doom-font (font-spec :family "Fira Code" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(setq enable-local-variables 't)

(exec-path-from-shell-initialize)

(unless (am/phone-p)
  (use-package! keychain-environment
    :init
    (keychain-refresh-environment))

  (use-package! web-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
    :custom
    (web-mode-code-indent-offset 2)
    (web-mode-alist '(("go" . "\\.tmpl\\'")))))

(defvar am/buffer-name-max 50
  "The maximal length of the buffer name in modeline.")

(defun am/doom-modeline-segment--buffer-info ()
"Almost the same as `doom-modeline-segment--buffer-info',
but it truncates the buffer name within `am/buffer-name-max'."
(concat
  (s-truncate
    am/buffer-name-max
    (format-mode-line (doom-modeline-segment--buffer-info))
    "...")))

(defun am/doom-modeline-segment--org-clock ()
  "Displays org-mode-clock"
      (if (and (org-clocking-p) (doom-modeline--active))
    org-mode-line-string))

(after! doom-modeline
  (add-to-list 'doom-modeline-fn-alist (cons 'am/buffer-info 'am/doom-modeline-segment--buffer-info))
  (add-to-list 'doom-modeline-fn-alist (cons 'am/org-clock 'am/doom-modeline-segment--org-clock))
  (doom-modeline-def-modeline 'main
    '(bar matches am/org-clock am/buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs))
  (doom-modeline-def-modeline 'vcs
    '(bar window-number modals matches am/org-clock buffer-info buffer-position parrot selection-info)
    '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process)))

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
          (insert "  ; cleared on date:" date-string)))))))

(use-package! ledger-mode
  :mode "\\.journal\\'"
  :config
  (map! :after ledger-mode
        :mode ledger-mode
        :map ledger-mode-map
        :leader
        "m e" #'am/ledger-insert-effective-date
        "m t" (cmd! (ledger-toggle-current 'pending))
        "m s" #'ledger-sort-buffer)
  (setq ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-binary-path "hledger"
        ledger-post-account-alignment-column 2
        ledger-post-amount-alignment-column 0))

(after! evil
  (setq
   evil-shift-width 2
   evil-kill-on-visual-paste nil))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (map! :leader
        "n A" (cmd! (org-agenda nil "a")))
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Next" :tag "next" :todo t)
          (:habit t))))

(after! org
  (add-to-list 'org-modules 'org-habit t)
  (org-clock-persistence-insinuate)
  (setq
   org-directory "~/org"
   org-default-notes-file "~/org/notes.org"
   org-agenda-files '("~/org/gtd")
   org-archive-location "~/org/gtd/archive/archive.org::* From %s"
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-refile-targets '((org-agenda-files . (:maxlevel . 1)))
   ;; org-capture-templates '(("t" "todo" entry (file "~/org/gtd/refile.org") "* TODO %?"))
   org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)"))
   org-return-follows-link t
   ;; agenda configs
   calendar-week-start-day 1
   org-agenda-start-day nil
   org-deadline-warning-days 7
   org-duration-format '((special . h:mm))
   ;; org-clock-clocked-in-display nil ;; see implementation above for doom-modeline
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-clock-persist 'history
   org-habit-show-habits-only-for-today nil
   org-agenda-show-future-repeats nil
   org-agenda-custom-commands '(("a" "Super-agenda view"
                                 ((agenda "" ((org-agenda-span 'week)
                                              (org-super-agenda-groups
                                               '((:time-grid t)))))
                                  (alltodo "" ((org-agenda-overriding-header "")
                                               (org-agenda-block-separator "-")
                                               (org-super-agenda-groups
                                                '(
                                                  (:name "Inbox"
                                                   :tag "inbox")
                                                  (:name "Next"
                                                   :tag "next")
                                                  (:name "Weekend"
                                                   :tag "weekend")
                                                  (:name "Work"
                                                   :tag "work")
                                                  (:name "Orders"
                                                   :tag "orders")
                                                  (:name "Vacation"
                                                   :tag "vacation")
                                                  ;; :auto-tags t)
                                                  ;; (:name "Active films"
                                                  ;;        :todo ("ACTIVE" "TOWATCH"))
                                                  (:discard (:anything t))))))))))
  (use-package! doct
    :config
    (setq org-capture-templates (doct '("Inbox" :keys "i"
                                        :file "~/org/gtd/inbox.org"
                                        :template "* TODO %?")))))

(use-package! org-edna
  :config
  (org-edna-mode))

(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/code/"))
  (setq projectile-git-submodule-command nil))

(unless (am/phone-p)
  (add-hook 'compilation-finish-functions
            (lambda (_buf str)
              (if (null (string-match ".*exited abnormally.*" str))
                  ;;no errors, make the compilation window go away in a few seconds
                  (progn
                    (run-at-time
                     "1 sec" nil 'delete-windows-on
                     (get-buffer-create "*compilation*<ml>"))
                    (message "No Compilation Errors!")))))

  (use-package! yequake
    :custom
    (yequake-frames
     '(("org-capture"
        (buffer-fns . (yequake-org-capture))
        (width . 0.75)
        (height . 0.5)
        (alpha . 0.95)
        (frame-parameters . ((undecorated . t)
                             (skip-taskbar . t)
                             (sticky . t)))))))

  (use-package! lsp-java
    :init
    (setq lsp-java-java-path "/usr/lib/jvm/java-11-openjdk/bin/java")
    (setq lsp-java-format-settings-url "file://home/komrad/.leanix-java-formatting.xml")
    (setq lsp-java-format-settings-profile "LeanixFlavoredGoogleStyle")
    (setq lsp-java-save-actions-organize-imports t))

  (use-package! forge
    :after magit)

  (use-package company
    :config
    (setq company-shell-modes '()))

  (add-hook! sh-mode
    (set-company-backend! 'sh-mode nil))

  (use-package flycheck-clang-tidy
    :after flycheck
    :hook
    (flycheck-mode . flycheck-clang-tidy-setup))

  (setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))

  (after! lsp-clangd (set-lsp-priority! 'clangd 2))

  (setq lsp-zig-zls-executable "~/zls/zig-out/bin/zls")
  (setq +format-with-lsp nil)
  (setq c-basic-offset 2)

  (after! magit (progn (setq magit-prefer-remote-upstream 't)
                       (setq git-commit-style-convention-checks
                             (remove 'overlong-summary-line git-commit-style-convention-checks))))
)

(if (eq system-type 'darwin)
    (mac-auto-operator-composition-mode))

(setq org-roam-directory "~/org/roam")
