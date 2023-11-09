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
(setq doom-font (font-spec :family "Jetbrains Mono" :size 24 :weight 'bold))

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

(when (or (memq window-system '(mac ns x)) (daemonp))
  (exec-path-from-shell-initialize))

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
  (org-super-agenda-mode))

(after! org
  (add-to-list 'org-modules 'org-habit t)
  (org-clock-persistence-insinuate)
  (setq!
   org-directory "~/org"
   org-default-notes-file "~/org/notes.org"
   org-agenda-files '("~/org/gtd")
   org-archive-location "~/org/gtd/archive/archive.org::* From %s"
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-refile-targets '((org-agenda-files . (:maxlevel . 1)))
   ;; org-capture-templates '(("t" "todo" entry (file "~/org/gtd/refile.org") "* TODO %?"))
   org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)"))
   org-todo-keyword-faces '(("WAITING" . "orange") ;; orders
                            ("TOSEND" . "salmon")
                            ("SENT". "olive drab")
                            ("RECEIVED" . "chocolate")
                            ("ACTIVE" . "orange") ;; films (+work)
                            ("READY" . "pink")
                            ("BLOCKED" . "red")) ;; work
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
   org-agenda-custom-commands '(("p" "Personal super-agenda"
                                 ((agenda "" ((org-agenda-span 'day)
                                              (org-super-agenda-groups
                                               '((:name "Today"
                                                  :time-grid t)
                                                 (:name "Habits"
                                                  :habit t)))))
                                  ;; (agenda "" ((org-agenda-start-on-weekday nil)
                                  ;;             (org-agenda-start-day "+1d")
                                  ;;             (org-agenda-span 6)
                                  ;;             (org-super-agenda-groups
                                  ;;              '(:time-grid t))))
                                  (todo "" ((org-agenda-overriding-header "")
                                            (org-agenda-block-separator "---")
                                            (org-super-agenda-groups
                                             '(
                                               (:name "Inbox"
                                                :tag "inbox")
                                               ;; (:name "Work"
                                               ;;  :tag "work")
                                               (:name "Next"
                                                :tag "next")
                                               ;; (:name "Weekend"
                                               ;;  :tag "weekend")
                                               ;; (:name "Work"
                                               ;;  :and (:tag "work" :not (:tag "someday")))
                                               (:name "Orders"
                                                :tag "orders")
                                               ;; (:name "Vacation"
                                               ;;  :tag "vacation")
                                               ;; :auto-tags t)
                                               ;; (:name "Active films"
                                               ;;        :todo ("ACTIVE" "TOWATCH"))
                                               (:discard (:anything t))))))))
                                ("w" "Work super-agenda"
                                 ((alltodo "" ((org-agenda-overriding-header "")
                                               (org-agenda-block-separator "---")
                                               (org-super-agenda-groups
                                                '(
                                                  (:name "Work"
                                                   :and (:tag "work" :not (:tag "someday")))
                                                  (:discard (:anything t))))))))
                                ))

  (use-package! doct
    :config
    (setq org-capture-templates (doct '(
                                        ("todo" :keys "t"
                                         :file "~/org/gtd/inbox.org"
                                         :template "* %?")
                                        ("work" :keys "w"
                                         :file "~/org/gtd/work.org"
                                         :headline "Inbox"
                                         :template "* %?")
                                        ("books" :keys "b"
                                         :file "~/org/roam/20230626083237-books.org"
                                         :headline "Inbox"
                                         :template "* %?")
                                        )))))

(use-package! projectile
  :config
  (setq projectile-project-search-path '(("~/code" . 2)))
  (setq projectile-git-submodule-command nil))

;; set lsp-eslint-server-command

(if (eq system-type 'darwin)
    (mac-auto-operator-composition-mode))

(setq org-roam-directory "~/org/roam")

(setq! org-reverse-note-order 't)
(add-hook! 'org-insert-heading-hook (save-excursion ;; TODO: extract into separate function to provide a name
                                      ;; TODO: only insert if "TODO" keyword is found
                                      ;; TODO: make it work for org-capture too
                                      (org-back-to-heading)
                                      (org-set-property "CREATED" (format-time-string (org-time-stamp-format 't 't)))))

(defun dlukes/ediff-doom-config (file)
  "ediff the current config with the examples in doom-emacs-dir

There are multiple config files, so FILE specifies which one to
diff.

Taken from here: https://github.com/doomemacs/doomemacs/issues/581#issuecomment-895462086
"
  (interactive
   (list (read-file-name "Config file to diff: " doom-user-dir)))
  (let* ((stem (file-name-base file))
         (customized-file (format "%s.el" stem))
         (template-file-regex (format "^%s.example.el$" stem)))
    (ediff-files
     (concat doom-user-dir customized-file)
     (car (directory-files-recursively
           doom-emacs-dir
           template-file-regex
           nil
           (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))))

(unless IS-ANDROID
  (use-package! org-edna
    :config
    (org-edna-mode))

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
        (width . 0.125)
        (height . 0.5)
        ;; (alpha . 0.95)
        (frame-parameters . ((undecorated . t)
                             (skip-taskbar . t)
                             (sticky . t)))))))

  (use-package! lsp-java
    :init
    ;; (setq lsp-java-java-path "/usr/lib/jvm/java-11-openjdk/bin/java")
    ;; (setq lsp-java-format-settings-url "file://home/komrad/.leanix-java-formatting.xml")
    ;; (setq lsp-java-format-settings-profile "LeanixFlavoredGoogleStyle")
    (setq lsp-java-save-actions-organize-imports t))

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

  (setq +format-with-lsp nil)
  (setq c-basic-offset 2)

  (after! magit (progn (setq magit-prefer-remote-upstream 't)
                       (setq git-commit-style-convention-checks
                             (remove 'overlong-summary-line git-commit-style-convention-checks))))

  (use-package! tts-editor
    :commands (tts-editor/listen-start
               tts-editor/listen-stop))

  (add-hook! python-mode
    (setq lsp-python-ms-executable (executable-find "python-language-server")))

  (setq-hook! 'zig-mode-hook lsp-zig-zls-executable (executable-find "zls")
              zig-format-on-save nil
              +format-with-lsp 't)

  ;; (add-hook! '(typescript-mode-local-vars-hook
  ;;              css-mode-hook
  ;;              json-mode-local-vars-hook)
  ;;   (setq lsp-eslint-node (executable-find "node")))

  (use-package! websocket
    :after org-roam)

  (use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  (use-package! org-ql
    :after org
    :custom
    (org-ql-search-directories-files-recursive t))

  (use-package! org-now
    :after org
    :config
    (;; setq! org-now-location (concat org-directory "/org-now.org")
     map! :leader
     :mode org-mode
     "m r n l" #'org-now-link
     "m r n n" #'org-now-refile-to-now
     "m r n p" #'org-now-refile-to-previous-location))

  (use-package! keychain-environment
    :init
    (keychain-refresh-environment))

  (use-package! web-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
    :custom
    (web-mode-code-indent-offset 2)
    (web-mode-alist '(("go" . "\\.tmpl\\'"))))
  (use-package! vcl-mode))

(when IS-ANDROID
  (setq browse-url-browser-function 'browse-url-xdg-open)
  (use-package! xclip
    :config
    (xclip-mode 1)))
