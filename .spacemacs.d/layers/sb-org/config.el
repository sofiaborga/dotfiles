(with-eval-after-load 'org
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  ;;(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)"
                                    "|" "DONE(d)" "CANCELED(c)"))
      ;;Tags (global)
      org-tag-alist '((:startgroup . nil)
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

        org-tags-exclude-from-inheritance '("@PROJEKT")
        org-tags-exclude-from-inheritance '("@PROJEKT")
        org-agenda-files '("~/GTD/master.org"
                           "~/GTD/tickler.org")
        )

;;Agenda setup
  (setq org-agenda-files '("~/GTD/master.org"
                           "~/GTD/tickler.org"))

  (setq org-deadline-warning-days 14)

  (setq org-agenda-custom-commands
        '(("d" "Daily overview"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Most Important Things")))
          (agenda "")
          (tags-todo "+@DATAWORK-@LÄSA-@WEBSURF")
          (tags-todo "@TELEFONEN")
          (tags-todo "@DATAWORK+@SPRINT")
          (tags-todo "@DATAWORK+@LÄSA")
          (tags-todo "@DATAWORK+@WEBSURF")))

          ("f" "Free time tasks"
           ((tags-todo "Ben")
            (tags-todo "@TELEFONEN")
            (tags-todo "@FRITID")
            (tags-todo "@LÄSA-@DATAWORK")
            (tags-todo "@SPRINT-@DATAWORK")
            (tags-todo "@WEBSURF-@DATAWORK")))
	
          ("V" "Veckogenomgång"
           ((agenda)
            (tags "@DATAWORK")
            (tags "@FRITID")
            (tags "@TELEFONEN")
            (tags "@LÄSA")
            (tags "@WEBSURF")
            (tags "@ÄRENDEN")
            (tags "Ben")
            (tags "@PROJEKT")
            (tags "@Sverige")))))
  ;;Refile targets
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '(("~/GTD/master.org" :maxlevel . 6)
                             ("~/GTD/someday.org" :level . 1)
                             ("~/GTD/tickler.org" :maxlevel . 1)
                             ("~/GTD/bibliotek.org" :maxlevel . 4)
                             ("~/GTD/andas.org" :level . 1)))

  ;;Capture setup
  (setq org-capture-templates
        '(("z" "zettel" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)

          ("i" "Inbox entry" entry
           (file "~/GTD/andas.org")
           "* %?")))



(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;;(add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell      . t)
                               (emacs-lisp . t)
                               (python     . t)
                               (jupyter    . t)
                               (dot        . t))))
