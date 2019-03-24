;;; emacs-reveal.el --- Create OER as reveal.js presentations with Emacs  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jens Lechtenbörger
;; URL: https://gitlab.com/oer/emacs-reveal
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.4") (oer-reveal "0.9.2") (org-re-reveal-ref "0.9.1"))
;; Keywords: hypermedia, tools, slideshow, presentation, OER

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
;; TODO
;; The software emacs-reveal allows to create HTML presentations (with
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
;; Create a directory for your presentation(s)' Org source files.  In
;; that directory:
;; (a) git clone https://gitlab.com/oer/emacs-reveal.git
;; (b) cd emacs-reveal
;; (c) Make sure that Emacs package org-re-reveal-ref is installed.
;;     - Install with the usual package mechanism or like
;;       this: emacs --batch --load install.el --funcall install
;;     - Or you could use this docker image:
;;       registry.gitlab.com/oer/docker/debian-emacs-tex-org:v3.3
;; (d) Add a line like this to ~/.emacs:
;;     (load "/path/to/emacs-reveal/emacs-reveal.el")
;; (e) Emacs-reveal suggests that reveal.js and several of its plugins
;;     are installed in the directory specified by customizable
;;     variable `emacs-reveal-submodules-dir'.
;;     Upon first loading in Emacs, emacs-reveal offers to download
;;     necessary software via Git for you.
;;     Also, (aiming for installation via MELPA one day) you may
;;     customize `emacs-reveal-generate-org-includes-p' to
;;     generate include files for shipped Org config files in the
;;     stable location specified by `emacs-reveal-org-includes-dir'.
;;
;; * Usage
;; Please check out the emacs-reveal howto mentioned above.  In
;; particular, the howto makes use of emacs-reveal-publish.el to
;; publish reveal.js presentations from Org source files (in file
;; elisp/publish.el).
;; Note that the HTML version of the howto is generated in a Docker
;; image on GitLab; its YML configuration shows the necessary steps
;; to generate and publish the project:
;; https://gitlab.com/oer/emacs-reveal-howto/blob/master/.gitlab-ci.yml
;;
;; Variable `emacs-reveal-dir' points to the directory of emacs-reveal
;; and its embedded plugins and resources.  You may want to use that
;; variable in your own publication code (similarly to its use in
;; elisp/publish.el of the howto).  Note that subdirectory
;; "title-slide" contains some variants for title slides of
;; presentations, and subdirectory "css" contains sample CSS.
;; Subdirectory "org" contains Org files to embed in presentations.
;; Please be warned that included resources, in particular
;; css/jl-simple.css, may change in incompatible ways.
;; You may want to work with your own copies.
;;

;;; Code:
(require 'oer-reveal-publish)
(oer-reveal-setup-submodules t)
(oer-reveal-generate-include-files t)
(oer-reveal-setup-plugins)
(oer-reveal-publish-setq-defaults)

;; Setup Bibliography in HTML based on default bib file (which helps to
;; locate the bib file when the current buffer does not specify one).
;; Display article, book, inproceedings differently.  Entry misc is new.
;; Remaining entries are defaults.
(require 'org-ref)
(require 'org-re-reveal-ref)
(setq org-ref-default-bibliography '("references.bib")
      org-ref-bibliography-entry-format
      '(("article" . "%a, %t, <i>%j %v(%n)</i>, %p (%y). <a href=\"%U\">%U</a>")
	("book" . "%a, %t, %u, %y. <a href=\"%U\">%U</a>")
	("inproceedings" . "%a, %t, %b, %y. <a href=\"%U\">%U</a>")
	("incollection" . "%a, %t, %b, %u, %y. <a href=\"%U\">%U</a>")
	("misc" . "%a, %t, %i, %y.  <a href=\"%U\">%U</a>")
	("techreport" . "%a, %t, %i, %u (%y).")
	("proceedings" . "%e, %t in %S, %u (%y).")
	))

(provide 'emacs-reveal)
;;; emacs-reveal.el ends here
