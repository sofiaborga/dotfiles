;;; packages.el --- sb-org layer packages file for Spacemacs.
;;
;; Author: Sofia Borgå <sofia.borgaa@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; See the Spacemacs documentation and FAQs for instructions on
;; how to implement a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this
;; layer should be added to `sb-org-packages'. Then, for each
;; package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer,
;;   define a function `sb-org/init-PACKAGE' to load and
;;   initialize the package.
;;
;; - Otherwise, PACKAGE is already referenced by another
;;   Spacemacs layer, so define the functions
;;   `sb-org/pre-init-PACKAGE' and/or `sb-org/post-init-PACKAGE'
;;   to customize the package as it is loaded.
;;
;;; Code:

(defconst sb-org-packages
  '(org
    (org-projectile :excluded t)
    (org-pomodoro :excluded t)
    org-bullets
    graphviz-dot-mode
    (org-roam :location
               (recipe :fetcher github :repo "jethrokuan/org-roam"))
    ))

(defun sb-org/post-init-org ()
    (setq org-return-follows-link t
        org-hide-emphasis-markers t
        ;;org-src-fontify-natively t ;; Pretty code blocks
        ;;org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
       )

  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  ;;(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries
  (add-hook 'org-mode-hook 'yas-minor-mode-on)


  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))


;;(defun ha-org/post-init-org-journal ()
;;  "Additional configuration for org-journal."
;;  (use-package org-journal
;;    :ensure t))

(defun sb-org/post-init-org-bullets ()
  (add-hook 'org-mode-hook 'org-bullets-mode))

(defun sb-org/init-graphviz-dot-mode ()
  (use-package graphviz-dot-mode
    :ensure t
    :mode "\\.dot\\'"
    :init
    (setq tab-width 4
          graphviz-dot-indent-width 2
          graphviz-dot-auto-indent-on-newline t
          graphviz-dot-auto-indent-on-braces t
          graphviz-dot-auto-indent-on-semi t)))

(defun sb-org/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory "~/Notes/")
    :init
    (progn
      (spacemacs/declare-prefix "ar" "org-roam")
      (spacemacs/set-leader-keys
        "arl" 'org-roam
        "art" 'org-roam-today
        "arf" 'org-roam-find-file
        "arg" 'org-roam-show-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rl" 'org-roam
        "rt" 'org-roam-today
        "rb" 'org-roam-switch-to-buffer
        "rf" 'org-roam-find-file
        "ri" 'org-roam-insert
        "rg" 'org-roam-show-graph))))

