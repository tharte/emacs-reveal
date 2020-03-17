;;; emacs-reveal.el --- Create OER as reveal.js presentations with Emacs  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; SPDX-FileCopyrightText: 2017-2020 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jens Lechtenbörger
;; URL: https://gitlab.com/oer/emacs-reveal
;; Version: 7.1.1
;; Package-Requires: ((emacs "25.1") (oer-reveal "2.5.0") (org-re-reveal-ref "1.0.0"))
;; Keywords: hypermedia, tools, slideshow, presentation, OER

;; Note that package org-ref is transitively required by emacs-reveal
;; (org-re-reveal-ref requires org-ref).  As org-ref has lots of
;; depencies itself, those packages are not included here.  If org-ref
;; is missing, installation from MELPA is offered.

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
;; The software emacs-reveal helps to create HTML presentations (with
;; audio explanations if you wish) with reveal.js from Org mode files
;; in GNU Emacs with org-re-reveal (a fork of org-reveal) and several
;; reveal.js plugins.  Generated presentations are usable with standard
;; browsers, also mobile and offline.
;;
;; Emacs-reveal grew out of a forked version of org-reveal
;; (https://github.com/yjwen/org-reveal) when its development stopped.
;; This led to the creation of org-re-reveal, org-re-reveal-ref, and
;; oer-reveal, upon which emacs-reveal is built.
;;
;; Just as org-reveal, emacs-reveal provides an export back-end for Org
;; mode (see https://orgmode.org/manual/Exporting.html).  As such, it
;; comes with Org's separation of contents and layout, allowing to
;; create presentations in a fashion similarly to Beamer LaTeX,
;; including the use of BibTeX files for bibliographic notes (for
;; classroom presentations), hyperlinks within and between
;; presentations, and the generation of a keyword index.
;; Beyond other similar projects, emacs-reveal comes with mechanisms
;; (a) to add audio explanations to presentations and (b) to share
;; free and open images and figures with proper attribution
;; information for their inclusion in open educational resources
;; (OER).  See there for OER presentations (HTML with reveal.js and
;; PDF generated from Org files via LaTeX) for a university course on
;; Operating Systems that are generated with emacs-reveal:
;; https://oer.gitlab.io/OS/
;;
;; A howto for the use of emacs-reveal is available over there:
;; https://gitlab.com/oer/emacs-reveal-howto/blob/master/howto.org
;; https://oer.gitlab.io/emacs-reveal-howto/howto.html (generated HTML)
;;
;; * Installation
;; You need Git to use emacs-reveal.  See there:
;; https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
;;
;; Emacs-reveal is only available from GitLab.
;;
;; Create a directory for your presentation(s)' Org source files.
;; Somewhere, maybe in that directory install emacs-reveal and its
;; prerequisite packages:
;; (a) git clone https://gitlab.com/oer/emacs-reveal.git
;; (b) cd emacs-reveal
;; (c) Make sure that Emacs packages org-re-reveal-ref and oer-reveal
;;     are installed.
;;     - Install with the usual package mechanism or like
;;       this: emacs --batch --load install.el --funcall install
;;     - Or you could use this Docker image:
;;       registry.gitlab.com/oer/docker/emacs-reveal:1.1
;; (d) Add a line like this to ~/.emacs (beware, this changes settings
;;     of other packages) or to your publication code:
;;     (load "/path/to/emacs-reveal/emacs-reveal.el")
;;
;; * Usage
;; Please check out the emacs-reveal howto mentioned above.  In
;; particular, the howto contains a sample file "elisp/publish.el" to
;; publish reveal.js presentations from Org source files.
;; Note that the HTML version of the howto is generated in a Docker
;; image on GitLab; its YML configuration shows the necessary steps
;; to generate and publish the project:
;; https://gitlab.com/oer/emacs-reveal-howto/blob/master/.gitlab-ci.yml

;;; Code:
(package-initialize)

;; org-ref has f as dependency.  If f is missing, offer to install org-ref.
;; Do not require org-ref here as that might pull in a wrong Org version,
;; since load-path has not been set up yet.
(condition-case nil
    (require 'f)
  (error
   (unless
       (yes-or-no-p
        "Required package `f' for `org-ref' not found.  Install from MELPA? ")
     (error "Please install `org-ref' to use `emacs-reveal'"))
   (let ((package-archives '(("melpa" . "https://melpa.org/packages/"))))
     (package-refresh-contents)
     (package-install 'org-ref)
     (message-box "Installed `org-ref'.  Please restart Emacs to avoid issues with mixed Org installations."))))

(require 'f)
(defconst emacs-reveal-lisp-packages
  (list (f-join "org-mode" "lisp" "org-version.el")
        (f-join "org-re-reveal" "org-re-reveal.el")
        (f-join "org-re-reveal-ref" "org-re-reveal-ref.el")
        (f-join "oer-reveal" "oer-reveal.el"))
  "Lisp files of packages included as submodules.")

(defgroup org-export-emacs-reveal nil
  "Options for exporting Org files to reveal.js HTML pressentations.
The options here are provided by package `emacs-reveal'.  They extend those
of `oer-reveal'."
  :tag "Org Export emacs-reveal"
  :group 'org-export-oer-reveal)

(defcustom emacs-reveal-managed-install-p t
  "Configure whether to use update `emacs-reveal' and submodules or not.
By default, `emacs-reveal' tries to update itself and its submodules via Git.
If you set this to nil, you should install Lisp packages in
`emacs-reveal-lisp-packages' yourself."
  :group 'org-export-emacs-reveal
  :type 'boolean
  :package-version '(emacs-reveal . "7.0.0"))

(defvar emacs-reveal-install-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory of file \"emacs-reveal.el\".")

(defcustom emacs-reveal-default-bibliography '("references.bib")
  "Default bibliography to assign to `org-ref-default-bibliography'.
A default helps to locate the bib file when the current buffer does not
specify one."
  :group 'org-export-emacs-reveal
  :type '(repeat :tag "List of BibTeX files" file)
  :package-version '(emacs-reveal . "7.1.0"))

(defcustom emacs-reveal-bibliography-entry-format
  '(("article" . "%a, %t, <i>%j %v(%n)</i>, %p (%y). <a href=\"%U\">%U</a>")
    ("book" . "%a, %t, %u, %y. <a href=\"%U\">%U</a>")
    ("inproceedings" . "%a, %t, %b, %y. <a href=\"%U\">%U</a>")
    ("incollection" . "%a, %t, %b, %u, %y. <a href=\"%U\">%U</a>")
    ("misc" . "%a, %t, %i, %y.  <a href=\"%U\">%U</a>")
    ("phdthesis" . "%a, %t, %s, %y.  <a href=\"%U\">%U</a>")
    ("techreport" . "%a, %t, %i, %u (%y).")
    ("proceedings" . "%e, %t in %S, %u (%y)."))
  "Value to assign to `org-ref-bibliography-entry-format'.
This defines the layout of bibliography entries in presentations.
The default value displays article, book, inproceedings differently;
entries incollection, misc, and phdthesis are new, while entries
  techreport and proceedings are defaults of `org-ref'."
  :group 'org-export-emacs-reveal
  :type '(alist :key-type (string) :value-type (string))
  :package-version '(emacs-reveal . "7.1.0"))

(defun emacs-reveal-submodules-ok ()
  "Check whether submodules are initialized properly.
Check whether (a) Lisp files for submodules in `emacs-reveal-lisp-packages'
are readable and (b) the JavaScript file \"reveal.js\" is readable.
If a check fails, return nil; otherwise, return directory of `emacs-reveal'."
  (let ((revealjs (f-join emacs-reveal-install-dir "emacs-reveal-submodules"
                          "reveal.js" "js" "reveal.js")))
    (when (and
           (not (memq nil
                      (mapcar #'file-readable-p
                              (mapcar (lambda (file)
                                        (f-join emacs-reveal-install-dir file))
                                      emacs-reveal-lisp-packages))))
           (file-readable-p revealjs))
      emacs-reveal-install-dir)))

(add-to-list 'load-path (f-join emacs-reveal-install-dir "git-invoke"))
(require 'git-invoke)
(defun emacs-reveal-setup ()
  "Set up `emacs-reveal'.
If `emacs-reveal-managed-install-p' is t, update submodules.
If submodules are present, add directories of Lisp packages to `load-path'."
  (when emacs-reveal-managed-install-p
    (if (file-readable-p (f-join emacs-reveal-install-dir ".git"))
        (git-invoke-update-submodules emacs-reveal-install-dir)
      ;; Submodules might still be OK, e.g., in Docker.  Raise error if not.
      (unless (emacs-reveal-submodules-ok)
        (error "Must have a \".git\" subdirectory for managed install of `emacs-reveal'"))))
  (when (emacs-reveal-submodules-ok)
    (dolist (file emacs-reveal-lisp-packages)
      (add-to-list 'load-path (f-join emacs-reveal-install-dir
                                      (file-name-directory file))))))

(defun emacs-reveal-setup-oer-reveal ()
  "Set up `oer-reveal' for use with `emacs-reveal'.
If `oer-reveal' is used standalone, it manages installation and updating
of \"emacs-reveal-submodules\" itself.  When used as part of
`emacs-reveal' with properly installed submodules, `oer-reveal' should
not touch submodules.  Thus, set `oer-reveal-submodules-dir' to its
subdirectory under `emacs-reveal' and set
`oer-reveal-submodules-version' to nil.
Call `oer-reveal-setup-submodules', `oer-reveal-generate-include-files',
and `oer-reveal-publish-setq-defaults'."
  (when (emacs-reveal-submodules-ok)
    (let ((dir (f-join emacs-reveal-install-dir "emacs-reveal-submodules")))
      (setq oer-reveal-submodules-dir dir
            oer-reveal-submodules-version nil)))
  (oer-reveal-setup-submodules t)
  (oer-reveal-generate-include-files t)
  (oer-reveal-publish-setq-defaults))

;; Possibly update emacs-reveal (depending on emacs-reveal-managed-install-p);
;; set up load-path if necessary directories are present.
;; Afterwards, set up oer-reveal.
(emacs-reveal-setup)
(require 'oer-reveal-publish)
(emacs-reveal-setup-oer-reveal)

;; Set up bibliography in HTML.
(require 'org-ref)
(require 'org-re-reveal-ref)
(setq org-ref-default-bibliography emacs-reveal-default-bibliography
      org-ref-bibliography-entry-format emacs-reveal-bibliography-entry-format)

(provide 'emacs-reveal)
;;; emacs-reveal.el ends here
