;;; emacs-reveal-publish.el --- Publish reveal.js presentations from Org sources
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License: GPLv3

;;; Commentary:
;; This file contains setup code to publish reveal.js presentations
;; from Org source files with emacs-reveal.
;;
;; After loading this file, the standard Org export function
;; `org-publish-all' publishes all projects added by code below to
;; `org-publish-project-alist'.  Org source files (except explicitly
;; excluded ones) are published according to
;; `emacs-reveal-publish-org-publishing-functions'.  The target
;; directory is "public".  Besides, reveal.js resources are copied to
;; "public".
;;
;; If file "index.org" is present in the current directory (e.g., to
;; collect links to generated reveal.js presentations), it is added to
;; `org-publish-project-alist' for export to HTML (with the standard
;; HTML export back-end).  HTML export is modified by
;; `emacs-reveal-publish-html-doctype' and
;; `emacs-reveal-publish-html-postamble'.
;; Existing directories among "audio", "figures", "quizzes" are added
;; to `org-publish-project-alist' for export as attachment (copy).
;;
;; You may want to load/require this file from your own publish.el
;; with additional entries added to `org-publish-project-alist'.
;; Then, invoke publication based on your own publish.el:
;; emacs --batch --load publish.el --funcall org-publish-all
;;
;; Warning! By default, code below sets `org-confirm-babel-evaluate'
;; to `emacs-reveal-publish-confirm-evaluate', which defaults to nil.
;; This enables automatic execution of code embedded in Org source
;; files.  This may be dangerous, and I do *not* recommend this for
;; general Emacs sessions.  Set
;; `emacs-reveal-publish-confirm-evaluate' to t to be asked for
;; confirmation.
;;
;; Inspired by publish.el by Rasmus:
;; https://gitlab.com/pages/org-mode/blob/master/publish.el

;;; Code:
(package-initialize)
(require 'org)
(require 'ox-publish)
(require 'emacs-reveal)

;; The following colors are based on the tango custom theme.
;; Used for syntax highlighting.
(custom-set-faces
 '(default                      ((t (:foreground "#2e3436"))))
 '(font-lock-builtin-face       ((t (:foreground "#75507b"))))
 '(font-lock-comment-face       ((t (:foreground "#5f615c"))))
 '(font-lock-constant-face      ((t (:foreground "#204a87"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#a40000"))))
 '(font-lock-keyword-face       ((t (:foreground "#346604"))))
 '(font-lock-string-face        ((t (:foreground "#5c3566"))))
 '(font-lock-type-face          ((t (:foreground "#204a87"))))
 '(font-lock-variable-name-face ((t (:foreground "#b35000"))))
 )

(defcustom emacs-reveal-publish-confirm-evaluate nil
  "Value to assign to `org-confirm-babel-evaluate' before export.
The default is nil, which may be dangerous is not recommended for
general Emacs sessions."
  :group 'emacs-reveal
  :type 'boolean)
(setq org-confirm-babel-evaluate emacs-reveal-publish-confirm-evaluate)

(defcustom emacs-reveal-publish-org-publishing-functions
  '(org-re-reveal-publish-to-reveal org-latex-publish-to-pdf)
  "Functions to publish Org source files.
By default, Org files are published as reveal.js presentations and as
PDF.  For the latter, `org-latex-pdf-process' is modified in emacs-reveal-publish"
  :group 'emacs-reveal
  :type '(repeat function))

(defcustom emacs-reveal-publish-pdf-process
  '("latexmk -outdir=%o -interaction=nonstopmode -shell-escape -bibtex -pdf %f")
  "Value to assign to `org-latex-pdf-process' before export.
Set to nil to avoid an assignment."
  :group 'emacs-reveal
  :type '(choice (const nil) (repeat string)))

(defcustom emacs-reveal-publish-html-doctype "html5"
  "Value to assign to `org-html-doctype' before export.
Set to nil to avoid an assignment."
  :group 'emacs-reveal
  :type '(choice (const nil) string))

(defcustom emacs-reveal-publish-html-postamble
  "<p class=\"author\">License: This text, “<span property=\"dc:title\">%t</span>,” by <span property=\"dc:creator cc:attributionName\">%a</span> is published under the Creative Commons license <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a>.</p>
<p class=\"date\">Created: <span property=\"dc:created\">%C</span></p>
<div class=\"legalese\"><p><a href=\"/imprint.html\">Imprint</a> | <a href=\"/privacy.html\">Privacy Policy</a></p></div>"
  "Value to assign to `org-html-postamble' before export.
The default generates CC BY-SA 4.0 license information and links to
imprint and privacy policy.
Set to nil to avoid an assignment."
  :group 'emacs-reveal
  :type '(choice (const nil) string))

(when emacs-reveal-publish-pdf-process
  (setq org-latex-pdf-process emacs-reveal-publish-pdf-process))
(when emacs-reveal-publish-html-doctype
  (setq org-html-doctype emacs-reveal-publish-html-doctype))
(when emacs-reveal-publish-html-postamble
  (setq org-html-postamble emacs-reveal-publish-html-postamble))

;; Export different parts of Org presentations to sub-directory
;; "public".  Org presentations are exported according to
;; `emacs-reveal-publish-org-publishing-functions'.
;; Other parts are just copied with `org-publish-attachment'.
(setq org-publish-project-alist
      (list
       (list "org-presentations"
	     :base-directory "."
	     :base-extension "org"
	     :exclude "index\\|backmatter\\|config\\|course-list\\|license-template\\|imprint\\|privacy"
	     :publishing-function emacs-reveal-publish-org-publishing-functions
	     :publishing-directory "./public")
       (list "title-slide"
	     :base-directory (expand-file-name "title-slide" emacs-reveal-dir)
	     :base-extension (regexp-opt '("png" "jpg" "svg"))
	     :publishing-directory "./public/title-slide/"
	     :publishing-function 'org-publish-attachment)
       (list "reveal-theme"
	     :base-directory (expand-file-name "css" emacs-reveal-dir)
	     :base-extension 'any
	     :publishing-directory "./public/reveal.js/css/theme"
	     :publishing-function 'org-publish-attachment)
       (list "reveal-static"
	     :base-directory (expand-file-name
			      "reveal.js" emacs-reveal-submodules-dir)
	     :exclude "\\.git"
	     :base-extension 'any
	     :publishing-directory "./public/reveal.js"
	     :publishing-function 'org-publish-attachment
	     :recursive t)
       (list "reveal.js-jump-plugin"
	     :base-directory (expand-file-name
			      "reveal.js-jump-plugin/jump"
			      emacs-reveal-submodules-dir)
	     :base-extension 'any
	     :publishing-directory "./public/reveal.js/plugin/jump"
	     :publishing-function 'org-publish-attachment
	     :recursive t)
       (list "reveal.js-plugins-anything"
	     :base-directory (expand-file-name
			      "reveal.js-plugins/anything"
			      emacs-reveal-submodules-dir)
	     :base-extension 'any
	     :publishing-directory "./public/reveal.js/plugin/anything"
	     :publishing-function 'org-publish-attachment
	     :recursive t)
       (list "reveal.js-plugins-audio-slideshow"
	     :base-directory (expand-file-name
			      "reveal.js-plugins/audio-slideshow"
			      emacs-reveal-submodules-dir)
	     :base-extension 'any
	     :publishing-directory "./public/reveal.js/plugin/audio-slideshow"
	     :publishing-function 'org-publish-attachment
	     :recursive t)
       (list "reveal.js-quiz-plugin"
	     :base-directory (expand-file-name
			      "reveal.js-quiz/quiz"
			      emacs-reveal-submodules-dir)
	     :base-extension 'any
	     :publishing-directory "./public/reveal.js/plugin/quiz"
	     :publishing-function 'org-publish-attachment
	     :recursive t)
       (list "reveal-toc-plugin"
	     :base-directory (expand-file-name
			      "Reveal.js-TOC-Progress/plugin"
			      emacs-reveal-submodules-dir)
	     :base-extension 'any
	     :publishing-directory "./public/reveal.js/plugin"
	     :publishing-function 'org-publish-attachment
	     :recursive t)
       ))

(when (file-exists-p "index.org")
  (add-to-list 'org-publish-project-alist
	       (list "index"
		     :base-directory "."
		     :include '("index.org")
		     :exclude ".*"
		     :publishing-function '(org-html-publish-to-html)
		     :publishing-directory "./public")))
(when (file-accessible-directory-p "audio")
  (add-to-list 'org-publish-project-alist
	       (list "audio"
		     :base-directory "audio"
		     :base-extension (regexp-opt '("ogg" "mp3"))
		     :publishing-directory "./public/audio"
		     :publishing-function 'org-publish-attachment)))
(when (file-accessible-directory-p "figures")
  (add-to-list 'org-publish-project-alist
	       (list "figures"
		     :base-directory "figures"
		     :base-extension (regexp-opt '("png" "jpg" "ico" "svg" "gif"))
		     :publishing-directory "./public/figures"
		     :publishing-function 'org-publish-attachment
		     :recursive t)))
(when (file-accessible-directory-p "quizzes")
  (add-to-list 'org-publish-project-alist
	       (list "quizzes"
		     :base-directory "quizzes"
		     :base-extension (regexp-opt '("js"))
		     :publishing-directory "./public/quizzes"
		     :publishing-function 'org-publish-attachment)))

(provide 'emacs-reveal-publish)
;;; emacs-reveal-publish.el ends here
