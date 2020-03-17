;; .emacs.d/init.el
;; Author: Sofia Borgå, with inspiration from many many others.

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support

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
  
  ;;   Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired

(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  
  (when (< emacs-major-version 24)
    
    ;; For important compatibility libraries like cl-lib
    
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Initialize the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(doom-themes
    theme-changer               ;; For toggling day/night themes
    neotree                     ;; Nice-looking dir tree buffer
    all-the-icons               ;; Icons for neotree
    elpy                        ;; Emacs Lisp Python Env
    flycheck                    ;; Syntax checking
    py-autopep8                 ;; Run autopep8 on save
    org-bullets                 ;; Nicer bullets for org headings
    markdown-mode
    simple-httpd
    zmq
    jupyter                     
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; ===================================
;; Basic config
;; ===================================

(setq inhibit-splash-screen t)      ;; Disable the splash screen (0 for enable)

(global-linum-mode t)               ;; Enable line numbers globally

(transient-mark-mode 1)             ;; Enable transient mark mode

(add-hook 'text-mode-hook
	  (lambda ()
	    (variable-pitch-mode 1)))
                                    ;; Adding variable pitch mode to text
                                    ;; modes will help in rendering mixed
                                    ;; fonts every time you edit markdown,
                                    ;; org-mode, etc.

(setq calendar-location-name "München, DE") 
(setq calendar-latitude 48.13)
(setq calendar-longitude 11.57)

;; ===================================
;; Theme config
;; ===================================
(require 'theme-changer)
(change-theme 'doom-tomorrow-day 'doom-tomorrow-night)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
 
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
(doom-themes-treemacs-config)
  
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


;; ====================================
;; Development Setup
;; ====================================
(elpy-enable)

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")


;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;;====================================
;; Org Mode Config
;;====================================

;;Installing dev version of org-mode OR...
(setq load-path (cons "~/src/Emacs/org-mode/lisp" load-path))
(setq load-path (cons "~/src/Emacs/org-mode/contrib/lisp" load-path))

(require 'org-install)
(require 'org)

;;; To update org-mode:
;;;cd ~/Build/Emacs/org-mode
;;;git pull && make clean && make && make doc

;; ...OR Enable org mode
;;(require 'org)
;;(org-reload)

;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;==== VISUAL CONFIG =====
;; Use visual-line-mode for org buffers
(add-hook 'org-mode-hook 'visual-line-mode)

;; ;; Use variable-pitch-mode for org buffers
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; ;; Variable font etc depending on heading level
;; (custom-theme-set-faces
;;  'user
;; '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
;; '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))


;; Font config
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;; Nicer bullets for headings
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Nicer bullets for sublists
(font-lock-add-keywords 'org-mode
       '(("^ +\\([-*]\\) "
       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;;==== FUNCTIONAL CONFIG ====

;;Helper variables
(defvar org-default-GTD-dir        "~/GTD"                    "Primary GTD directory")
;;(defvar org-default-technical-dir  "~/technical"                    "Directory of shareable notes")
(defvar org-default-personal-file   "~/GTD/bibliotek.org"                     "Directory of un-shareable, personal notes")
;;(defvar org-default-completed-dir  "~/projects/trophies"            "Directory of completed project files")
;;(defvar org-default-inbox-file     "~/GTD/andas.org"          "New stuff collects in this file")
;;(defvar org-default-tasks-file     "~/GTD/master.org"         "Tasks, TODOs and little projects")
(defvar org-default-incubate-file  "~/GTD/someday.org"        "Ideas simmering on back burner")
(defvar org-default-tickler-file   "~/GTD/ticker.org"         "Future reminders are kept here")
;;(defvar org-default-completed-file nil)
;;(defvar org-default-notes-file     "~/personal/general-notes.org"   "Non-actionable, personal notes")
;;(defvar org-default-media-file     "~/projects/media.org"           "White papers and links to other things to check out")


;;TODO states
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

;;Agenda setup
(setq org-agenda-files '("~/GTD/master.org"  
			  "~/GTD/tickler.org"))
;;(setq org-agenda-files '(org-default-tasks-file
;;			 org-default-tickler-file))

(setq org-deadline-warning-days 14)

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Most Important Things")))
          (agenda "")
          (alltodo "")))
      
        ("V" "Veckogenomgång"
	 ((agenda)
            (tags-todo "@SVÅRT")
            (tags-todo "@DATORN")
            (tags-todo "@TELEFONEN")
            (tags-todo "@LÄSA")
            (tags-todo "@LÄTT")
	    (tags-todo "@ÄRENDEN")
	    (tags-todo "@Ben")
	    (tags-todo "@Österby")
	    (tags-todo "@Sverige")))))


;;Capture target file
;;(setq org-default-notes-file org-default-inbox-file)
;;Capture templates

;;Refile targets
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '(("~/GTD/master.org" :maxlevel . 6)
                           ("~/GTD/someday.org" :level . 1)
                           ("~/GTD/tickler.org" :maxlevel . 1)
			   ("~/GTD/bibliotek.org" :maxlevel . 4)
			   ("~/GTD/andas.org" :level . 1)))


;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;;(global-set-key "\C-cc" 'org-capture)
;;(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key [f8] 'neotree-toggle)

;;
;;
;;
;;
;; USER_DEFINED init.el ENDS HERE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c7aa6fcf85e6fe4ea381733d9166d982bbc423af4a55c26f55f5d0cfb0ce1835" "4bdc0dfc53ae06323e031baf691f414babf13c9c9c35014dd07bb42c4db27c24" default)))
 '(package-selected-packages
   (quote
    (all-the-icons neotree theme-changer spacemacs-theme poet-theme org gnu-elpa-keyring-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif" :height 1.4))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif" :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif" :height 1.2))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#4d4d4c" :family "Sans Serif")))))
