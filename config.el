;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sofia Borgå"
      user-mail-address "john@doe.com")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/GTD/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(add-to-list 'exec-path "/usr/bin/sqlite3")

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

(after! latex

  :config

  (setq reftex-default-bibliography "~/Notes/refs/roam.bib")
  (setq +latex-viewers '(pdf-tools))
  (map! :map cdlatex-mode-map
        :i "TAB" #'cdlatex-tab)
  )
(after! org
  (require 'org-habit)

  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (python . t))
        org-confirm-babel-evaluate  nil
        org-babel-default-header-args:jupyter-python '((:session . "py")
                                                       (:kernel . "python3"))
        org-src-fontify-natively t)



  (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)"
                                      "|" "DONE(d)" "CANCELED(c)")))
  (setq org-log-done 'time)

  (setq org-tag-alist '((:startgroup . nil)
                              ("@DATAWORK" . ?d)
                              ("@FRITID" . ?f)
                              ("@PROJEKT" . ?p)
                              ("@SVERIGE" . ?S)
                              (:endgroup . nil)
                              ("@BJÖRNEN" . ?B)
                              ("@LÄSA" . ?l)
                              ("@ÄRENDEN" . ?a)
                              ("@WEBSURF" . ?w)
                              ("@TELEFONEN" . ?t)
                              ("@SPRINT" . ?s)
                              ("Ben" . ?b))

        org-tags-exclude-from-inheritance '("@PROJEKT"))


  (defvar org-default-projects-dir   "~/GTD/projekt"                     "Primary GTD directory")
  (defvar org-default-inbox-file     "~/GTD/andas.org"         "New stuff collects in this file")
  (defvar org-default-tasks-file     "~/GTD/master.org"           "Tasks, TODOs and little projects")
  (defvar org-default-incubate-file  "~/GTD/someday.org"        "Ideas simmering on back burner")

  
  (setq org-agenda-files '("~/GTD/andas.org"
                           "~/GTD/master.org"
                           "~/GTD/tickler.org"
                           "~/GTD/someday.org"
                           "~/GTD/ideas.org"
                           "~/GTD/projekt"))
  (setq org-agenda-file-regexp "^[a-z0-9-_]+.org")

  (setq org-agenda-custom-commands
        '(("d" "Daily"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Most Important Things")))
          (agenda "")
          (tags-todo "+@DATAWORK-@LÄSA-@WEBSURF" ((org-agenda-overriding-header "Work")))
          (tags-todo "@TELEFONEN" ((org-agenda-overriding-header "People")))
          (tags-todo "@SPRINT" ((org-agenda-overriding-header "SPRINT")))))

          ("f" "FREI-zeit"
            ((tags-todo "Ben" ((org-agenda-overriding-header "Ben")))
            (tags-todo "@TELEFONEN" ((org-agenda-overriding-header "People")))
            (tags-todo "@FRITID" ((org-agenda-overriding-header "Kreativt")))
            (tags-todo "@LÄSA-@DATAWORK" ((org-agenda-overriding-header "Läsa & fundera")))
            (tags-todo "@WEBSURF-@DATAWORK" ((org-agenda-overriding-header "Kolla (upp)")))))

          ("r" "Review"
           ((agenda)
            (todo "WAITING" ((org-agenda-overriding-header "WAITING")))
            (tags "@PROJEKT" ((org-agenda-overriding-header "Projekt")))
            (tags "Ben" ((org-agenda-overriding-header "@Ben")))
            (tags "@DATAWORK" ((org-agenda-overriding-header "Job")))
            (tags "@TELEFONEN" ((org-agenda-overriding-header "Connections")))
            (tags "@FRITID" ((org-agenda-overriding-header "FREI-zeit")))
            (tags "@LÄSA" ((org-agenda-overriding-header "Läsa & fundera")))
            (tags "@WEBSURF" ((org-agenda-overriding-header "Kolla (upp)")))
            (tags "@ÄRENDEN" ((org-agenda-overriding-header "Besorgungen")))
            (tags "@Sverige" ((org-agenda-overriding-header "Sverige")))))))

   (setq org-refile-use-outline-path 'file
         org-outline-path-complete-in-steps nil)

   (setq org-refile-targets '(("~/GTD/master.org" :maxlevel . 6)
                             ("~/GTD/someday.org" :level . 1)
                             ("~/GTD/tickler.org" :maxlevel . 1)
                             ("~/GTD/bibliotek.org" :maxlevel . 4)
                             ("~/GTD/andas.org" :level . 1)
                             ("~/GTD/ideas.org" :maxlevel . 3)
                             ("~/GTD/anki.org" :level . 1))))
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam--current-buffer)

  :init
  (setq org-roam-directory "~/Notes/")

  :config
  (setq org-roam-completion-everywhere nil)
  )


(use-package! anki-editor
  :after org
  :config
  (setq anki-editor-org-tags-as-anki-tags nil)

  (defun sb-anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))))


(use-package! org-download
  :after org
  :config
  ;; org-download use buffer-local variables. Set it individually in files. Otherwise, put things flatly in misc
  ;; folder. Source: https://github.com/yiufung/dot-emacs/blob/master/init.el
  (setq-default org-download-method 'directory
                org-download-image-dir "~/Pictures/to_anki"
                org-download-heading-lvl nil
                org-download-delete-image-after-download t
                org-download-screenshot-method "echo"
                org-download-screenshot-file "/tmp/screenshot.png"
                org-download-image-org-width 300
               ;; org-download-annotate-function (lambda (link) "") ;; Don't annotate
                )

  ;; My customized org-download to incorporate flameshot gui Workaround to setup flameshot, which enables annotation.
  ;; In flameshot, set filename as "screenshot", and the command as "flameshot gui -p /tmp", so that we always ends up
  ;; with /tmp/screenshot.png. Nullify org-download-screenshot-method by setting it to `echo', so that essentially we
  ;; are only calling (org-download-image org-download-screenshot-file).
  (defun sb-org-download-screenshot ()
    "Capture screenshot and insert the resulting file.
     The screenshot tool is determined by `org-download-screenshot-method'."
    (interactive)
    (let ((tmp-file "/tmp/screenshot.png"))
      (delete-file tmp-file)
      (call-process-shell-command "flameshot gui -p /tmp/")
      ;; Because flameshot exit immediately, keep polling to check file existence
      (while (not (file-exists-p tmp-file))
        (sleep-for 2))
      (org-download-image tmp-file))))


(use-package! org-ref
  :after org
  :config
    (setq org-ref-default-bibliography '("~/Notes/refs/roam.bib")
        org-ref-pdf-directory "~/Notes/refs/pdfs/"
        ;;org-ref-notes-function 'org-ref-notes-many-files
        org-ref-notes-directory "~/Notes/refs/")

    ;; from docs
    (setq org-ref-notes-function
       (lambda (thekey)
         (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
           (bibtex-completion-edit-notes
            (list (car (org-ref-get-bibtex-key-and-file thekey)))))))
    )

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config)
