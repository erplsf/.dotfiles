;;;; org-mode
;; mostly stolen from here http://doc.norang.ca/org-mode.html

(straight-use-package 'org)
(require 'org)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-log-done 'time)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red")
              ("NEXT" :foreground "royal blue")
              ("DONE" :foreground "forest green")
              ("WAITING" :foreground "orange")
              ("CANCELLED" :foreground "forest green"))))

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/inbox.org")

(setq org-agenda-files '("~/org/inbox.org"
			 "~/org/habits.org"
			 "~/org/gtd.org"
                         "~/org/tickler.org"
			 "~/org/brandslisten.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file "~/org/inbox.org")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/org/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)
			   ("~/org/someday.org" :maxlevel . 2)))

(setq org-agenda-custom-commands
      '(("x" agenda)
        ("n" todo "NEXT")))

(add-to-list 'org-modules 'org-habit t)
