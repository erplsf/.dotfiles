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

(setq doom-font (font-spec :family "Input Mono" :size 19 :weight 'light :height 140))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'tomorrow-night)

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

(setq enable-local-variables 'yes)

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
  :bind (:map ledger-mode-map ("C-c C-t" . #'am/ledger-insert-effective-date))
  :config
  (define-key! evil-normal-state-map
    (kbd "SPC m e") #'am/ledger-insert-effective-date)
  (setq ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-binary-path "hledger"
        ledger-post-account-alignment-column 2
        ledger-post-amount-alignment-column 0))

(setq evil-shift-width 2)

(use-package! org-super-agenda
  :after org-agenda
  :config
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
   am/org-refile-file "~/org/gtd/refile.org"
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
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-clock-persist 'history
   org-habit-show-habits-only-for-today nil
   org-agenda-show-future-repeats nil
   org-agenda-custom-commands '(("a" "Super custom view"
                                 ((agenda "" ((org-agenda-span 'week)
                                              (org-super-agenda-groups
                                               '((:time-grid t)))))
                                  (alltodo "" ((org-agenda-overriding-header "")
                                               (org-agenda-block-separator "-")
                                               (org-super-agenda-groups
                                                '((:name "Work"
                                                   :tag "work")
                                                  (:name "Next"
                                                   :tag "next")
                                                  (:name "Orders"
                                                   :tag "orders")
                                                  (:name "Refile"
                                                   :tag "refile")
                                                  ;; :auto-tags t)
                                                  ;; (:name "Active films"
                                                  ;;        :todo ("ACTIVE" "TOWATCH"))
                                                  (:discard (:anything t))))))))))
  (use-package! doct
    :config
    (setq org-capture-templates (doct '("Task" :keys "t"
                                        :file am/org-refile-file
                                        :template "* TODO %?")))))

(use-package! org-edna
  :config
  (org-edna-mode))
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
    (setq lsp-java-save-actions-organize-imports t)))
