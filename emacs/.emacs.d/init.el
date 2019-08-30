;;  bootstrap straight.el

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

(straight-use-package 'color-theme-sanityinc-tomorrow)
(require 'color-theme-sanityinc-tomorrow)

;; disable gui stuff TODO:

(tool-bar-mode -1)
(menu-bar-mode -1) 
;; (toggle-scroll-bar -1) ;; -> doesn't work in client/daemon
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
;; (add-to-list 'default-frame-alist
;;             '(vertical-scroll-bars . nil))

(blink-cursor-mode 0)

;; save backups in one folder
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
(setq backup-by-copying t)

;; set default font
(setq default-frame-alist '((font . "Hack 14")))

;; follow symlinks

(setq vc-follow-symlinks t)

;; install packages

(straight-use-package 'org)
(straight-use-package 'magit)
(straight-use-package 'helm)
(straight-use-package 'smex)
(straight-use-package 'page-break-lines)
(straight-use-package 'dashboard)
(straight-use-package 'company)
(straight-use-package 'ace-window)
(straight-use-package 'flycheck)
(straight-use-package 'avy)
(straight-use-package 'pdf-tools)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'terraform-mode)
(straight-use-package 'perspective)

(require 'perspective)
(persp-mode)

(setq tramp-default-method "ssh")

(setq auto-mode-alist
  (cons '("\\.tf$" . terraform-mode) auto-mode-alist))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(pdf-loader-install)

(global-set-key (kbd "C-:") 'avy-goto-char)

(add-hook 'after-init-hook 'global-flycheck-mode)

(global-set-key (kbd "M-o") 'ace-window)

(add-hook 'after-init-hook #'global-company-mode)

;; (require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq global-page-break-lines-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; helm config
(require 'helm-config)

;; package-specific config section
;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;;(setq org-todo-keywords
;;      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/gtd.org"
                         "~/org/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/org/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))

;; magit

(global-set-key (kbd "C-x g") 'magit-status)

;; TODO: slime

(straight-use-package 'slime)
(setq inferior-lisp-program "/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons)
(setq doom-modeline-icon t)

;; don't forget to run `M-x all-the-icons-install-fonts`

(straight-use-package 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-project-search-path '("/code/brandslisten/"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
