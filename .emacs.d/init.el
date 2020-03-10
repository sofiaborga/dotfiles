;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable MELPA packages
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
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Adding variable pitch mode to text modes will help in rendering mixed fonts every time you edit markdown, org-mode, etc.
(add-hook 'text-mode-hook
	  (lambda ()
	    (variable-pitch-mode 1)))

;;Theme
(require 'poet-theme)

;;(set-face-attribute 'default nil :family "Iosevka" :height 130)
;;(set-face-attribute 'fixed-pitch nil :family "Iosevka")
;;(set-face-attribute 'variable-pitch nil :family "Baskerville")



;;;
;;; Org Mode Config
;;;

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

;;(setq org-bullets-bullet-list
  ;;   '("◉" "○"))
;;(org-bullets 1)


;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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
(setq org-refile-targets '(("~/GTD/master.org" :maxlevel . 5)
                           ("~/GTD/someday.org" :level . 1)
                           ("~/GTD/tickler.org" :maxlevel . 1)
			   ("~/GTD/bibliotek.org" :maxlevel . 4)
			   ("~/GTD/andas.org" :level . 1)))


;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;;(global-set-key "\C-cc" 'org-capture)
;;(global-set-key "\C-cb" 'org-iswitchb)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c7aa6fcf85e6fe4ea381733d9166d982bbc423af4a55c26f55f5d0cfb0ce1835" "4bdc0dfc53ae06323e031baf691f414babf13c9c9c35014dd07bb42c4db27c24" default)))
 '(package-selected-packages (quote (poet-theme org gnu-elpa-keyring-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
