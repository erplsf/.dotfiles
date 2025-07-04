;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

(package! ledger-mode)
(package! org-super-agenda)
(package! doct)

;; ;; Fix for a bug: https://github.com/doomemacs/doomemacs/issues/6425
;; (package! transient
;;   :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
;;   :recipe (:host github :repo "magit/transient"))

;; (package! with-editor
;;   :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
;;   :recipe (:host github :repo "magit/with-editor"))

(unless IS-ANDROID
  (package! org-edna)
  (package! jsonnet-mode)
  (package! yaml-mode)
  (package! vterm)
  (package! keychain-environment)
  (package! go-mode)
  (package! web-mode)
  (package! terraform-mode)
  (package! hcl-mode)
  (package! yequake)
  (package! dockerfile-mode)
  (package! lsp-java)
  (package! yaml-mode)
  (package! mustache-mode)
  (package! powershell)
  (package! pkgbuild-mode)
  (package! nasm-mode)
  (package! rg)
  (package! exec-path-from-shell)
  (package! js2-refactor)
  (package! org-wild-notifier)
  (package! flycheck-clang-tidy)
  (package! rmsbolt)
  (package! tts-editor
    :recipe (:host github :repo "dangersalad/emacs-tts-editor"))
  (package! magit-annex)
  (package! git-annex)
  (package! gh-notify)
  ;; (package! emacsql-sqlite :built-in 'prefer)
  (package! org-roam-ui)
  (package! org-ql)
  (package! org-now
    :recipe (:host github :repo "alphapapa/org-now"))
  (package! vcl-mode)
  (package! tramp)
  (package! yaml-pro)
  (package! magit-todos))

(when IS-ANDROID
  (package! xclip))  ;; to sync with termux/android clipboard

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
