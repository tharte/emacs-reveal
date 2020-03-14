;;; git-invoke.el --- Elisp functions to invoke Git commands  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; SPDX-FileCopyrightText: 2020 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jens Lechtenbörger
;; URL: https://gitlab.com/oer/git-invoke
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: git, vc

;;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see http://www.gnu.org/licenses/ or write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package offers Elisp functions to invoke Git commands (clone,
;; pull, update of submodules).
;; Some commands invoke Git and direct output into git-invoke-buffer,
;; others return a string; see doc strings of functions below.
;; This is not meant for end users but for programmers.
;;
;; Functionality here is refactored from code in oer-reveal:
;; https://gitlab.com/oer/oer-reveal/-/blob/master/oer-reveal.el

;;; Code:
(require 'subr-x) ; string-trim, string-join

(defgroup git-invoke nil
  "Invoke Git commands from Elisp."
  :group 'tools)

(defcustom git-invoke-buffer "*git-invoke output*"
  "Name of buffer holding Git output."
  :group 'git-invoke
  :type 'string)

(defun git-invoke--invoke (as-string git-dir command &rest args)
  "Invoke Git COMMAND in GIT-DIR with ARGS.
If AS-STRING is t, return output as string with `shell-command-to-string'.
Otherwise, invoke `call-process' and direct output to `git-invoke-buffer'."
  (save-excursion
    (pop-to-buffer (get-buffer-create git-invoke-buffer) nil t)
    (let* ((git-dir (file-name-as-directory git-dir))
           (default-directory git-dir)
	   ;; In newer Emacsen, call-process starts in default-directory,
	   ;; which is what we want.  In Emacs 24.5.1, this does not happen.
	   ;; Instead, assign (dummy) filename to buffer, from which call-process
	   ;; obtains its directory.
	   (buffer-file-name (concat git-dir git-invoke-buffer)))
      (if as-string
          (with-temp-buffer
            (apply #'call-process "git" nil t nil command args)
            (string-trim
             (buffer-substring-no-properties (point-min) (point-max))))
        (apply #'call-process "git" nil t t command args)))))

(defun git-invoke-clone (git-dir url)
  "Perform Git clone from URL in GIT-DIR."
  (git-invoke--invoke nil git-dir "clone" url))

(defun git-invoke-checkout (git-dir tag)
  "Perform Git checkout of TAG in GIT-DIR."
  (git-invoke--invoke nil git-dir "checkout" tag))

(defun git-invoke-pull (git-dir)
  "Perform Git checkout master and pull in GIT-DIR."
  (git-invoke--invoke nil git-dir "checkout" "master")
  (git-invoke--invoke nil git-dir "pull"))

(defun git-invoke-update-submodules (git-dir)
  "Perform recursive update of submodules in GIT-DIR."
  (git-invoke--invoke nil git-dir "submodule" "sync" "--recursive")
  (git-invoke--invoke nil git-dir "submodule" "update" "--init" "--recursive"))

(defun git-invoke-get-tag (git-dir)
  "Return output of \"git describe --tags\" in GIT-DIR as string."
  (git-invoke--invoke t git-dir "describe" "--tags"))

(defun git-invoke-get-origin-url (git-dir)
  "Return output of \"git remote get-url origin\" in GIT-DIR as string."
  (git-invoke--invoke t git-dir "remote" "get-url" "origin"))

(defun git-invoke-get-toplevel (git-dir)
  "Return output of \"git rev-parse --show-toplevel\" in GIT-DIR as string."
  (git-invoke--invoke t git-dir "rev-parse" "--show-toplevel"))

(provide 'git-invoke)
;;; git-invoke.el ends here
