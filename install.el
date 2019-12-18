;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Use this file to install (and update) Lisp packages for
;; emacs-reveal from the package repository MELPA.  Alternatively, you
;; might use the Docker image emacs-reveal (see URL
;; `https://gitlab.com/oer/docker') or configure your `load-path' to
;; include submodules of emacs-reveal.  In those cases, this file is
;; not necessary.
;;
;; To install necessary packages from MELPA, invoke the
;; following from the directory containing this file:
;; emacs --batch --load install.el --funcall install
;;
;; To update, without batch mode for user interaction:
;; emacs --load install.el --funcall update
;; This invokes `package-list-packages' to list all available packages.
;; Type `U' to mark packages with newer versions.  Then type `x' and
;; confirm with "yes" to start the update process.

;;; Code:
(defun install ()
  "Install oer-reveal from MELPA."
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-refresh-contents)
  (package-install 'org-re-reveal-ref)
  (package-install 'oer-reveal)
  )

(defun update ()
  "Update MELPA packages with `package-list-packages'."
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-refresh-contents)
  (package-list-packages)
  )

;;; install.el ends here
