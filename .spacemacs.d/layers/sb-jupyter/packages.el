;;; packages.el --- sb-jupyter layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sofia Borgå <sofia@Sofias-ThinkPad-T450s>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `sb-jupyter-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sb-jupyter/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sb-jupyter/pre-init-PACKAGE' and/or
;;   `sb-jupyter/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sb-jupyter-packages
  '(python
    company
    (websocket :location (recipe
                           :repo "ahyatt/emacs-websocket"
                           :fetcher github))
    (simple-httpd :location (recipe
                              :repo "skeeto/emacs-web-server"
                              :fetcher github))
    (zmq :location (recipe
                    :fetcher github
                    :repo "dzop/emacs-zmq"
                    :files (:defaults "Makefile" "src")))
    (jupyter :location (recipe
                        :fetcher github
                        :repo "dzop/emacs-jupyter"
                        :files (:defaults "Makefile" "widget.html" "js"))))
  "The list of Lisp packages required by the sb-jupyter layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun sb-jupyter/post-init-python ())

(defun ab-jupyter/post-init-company ())

(defun sb-jupyter/init-emacs-websocket ()
  (use-package websocket
    :ensure t))

(defun sb-jupyter/init-simple-httpd ()
  (use-package simple-httpd
    :ensure t))

(defun sb-jupyter/init-zmq ()
  (use-package zmq
    :ensure t))

(defun sb-jupyter/init-jupyter ()
  (use-package jupyter
    :ensure t))

;;; packages.el ends here
