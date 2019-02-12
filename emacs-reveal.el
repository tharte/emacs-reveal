;;; emacs-reveal.el --- Configuration for and extension of org-re-reveal-ref  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jens Lechtenbörger
;; URL: https://gitlab.com/oer/emacs-reveal
;; Version: 0.9.6
;; Package-Requires: ((emacs "24.4") (org-re-reveal-ref "0.9.1"))
;;    Emacs 24.4 adds advice-add and advice-remove.  Thus, Emacs
;;    should not be older.
;;    Note that we use alist-get, introduced in Emacs 25.1.   However,
;;    Emacs 24.4 is still OK as org-re-reveal-ref requries org-ref which
;;    in turn requires pdf-utils, which defines alist-get if necessary.
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
;; The software emacs-reveal allows to create HTML presentations (with
;; audio explanations if you wish) with reveal.js from Org mode files
;; in GNU Emacs with org-re-reveal (a fork of org-reveal) and several
;; reveal.js plugins.  Generated presentations are usable with standard
;; browsers, also mobile and offline.
;;
;; Emacs-reveal grew out of a forked version of org-reveal
;; (https://github.com/yjwen/org-reveal) whose development seems to
;; have stalled.  This lead to the creation of org-re-reveal and
;; org-re-reveal-ref, upon which emacs-reveal is built.
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
;; Currently, emacs-reveal is only available from GitLab.
;;
;; Create a directory for your presentation(s)' Org source files.  In
;; that directory:
;; (a) git clone https://gitlab.com/oer/emacs-reveal.git
;; (b) cd emacs-reveal
;; (c) Install submodules
;;     - git submodule sync --recursive
;;     - git submodule update --init --recursive
;; (d) Make sure that Emacs package org-re-reveal-ref is installed.
;;     - Install with the usual package mechanism or like
;;       this: emacs --batch --load install.el --funcall install
;;     - Or you could use this docker image:
;;       registry.gitlab.com/oer/docker/debian-emacs-tex-org:v3.1
;; (e) Add a line like this to ~/.emacs:
;;     (load "/path/to/emacs-reveal/emacs-reveal.el")
;;
;; * Usage
;; Please check out the emacs-reveal howto mentioned above.  In
;; particular, the howto contains Lisp code to publish reveal.js
;; presentations from Org source files (in file elisp/publish.el).
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
;; * Customizable options
;; Variable `emacs-reveal-script-files' lists JavaScript files to load
;; when initializing reveal.js.  If you use the version of reveal.js
;; coming with emacs-reveal, changes should not be necessary.
;;
;; Variable `emacs-reveal-external-components' documents source
;; locations for Git submodules related to reveal.js.
;; Function `emacs-reveal-install' can be used to install those
;; components (although this is not necessary when you follow the
;; installation instructions).
;;
;; Variable `emacs-reveal-plugins' lists reveal.js plugins among
;; `emacs-reveal-external-components' to be activated.  Remove those
;; that you do not need.
;;
;; When generating image grids, `emacs-reveal-export-dir' specifies
;; the directory into which to generate CSS code.  This should
;; probably be the directory into which you publish your HTML code.
;; I set this to "./" before exporting with `C-c C-e r b'.
;; The names of CSS files are determined by
;; `emacs-reveal-css-filename-template'.

;;; Code:
(require 'cl-lib) ; cl-mapcar

;; Customizable options
(defcustom emacs-reveal-script-files '("js/reveal.js")
  "Value to apply to `org-re-reveal-script-files'.
By default, `org-re-reveal' also loads head.min.js, which has been removed
from the dev branch of reveal.js on 2018-10-04."
  :group 'emacs-reveal
  :type '(repeat string))

(defcustom emacs-reveal-external-components
  '(("https://github.com/hakimel/reveal.js.git" "master" javascript)
    ("https://github.com/e-gor/Reveal.js-TOC-Progress.git" "master" plugin)
    ("https://github.com/SethosII/reveal.js-jump-plugin.git" "master" plugin)
    ("https://github.com/rajgoel/reveal.js-plugins.git" "master" plugin)
    ("https://gitlab.com/schaepermeier/reveal.js-quiz.git" "master" plugin))
  "List of components to use with emacs-reveal, each with source and type.
Customize to add your own components.  Each entry is a list with three
elements:
First, a Git URI from which to clone the component.
Second, the Git branch to checkout.
Third, the type of the component, which can be `javascript' (to be
installed in any case), or `plugin' (to be installed if the component
occurs in `emacs-reveal-plugins').
This list documents source locations for Git submodules of
emacs-reveal."
  :group 'emacs-reveal
  :type '(repeat (list
		  (string :tag "Repository")
		  (string :tag "Branch")
		  (choice (const :tag "JavaScript library" javascript)
			  (const :tag "Configurable plugin" plugin)))))

(defcustom emacs-reveal-plugins
  '("reveal.js-plugins" "Reveal.js-TOC-Progress" "reveal.js-jump-plugin"
    "reveal.js-quiz")
  "List of `plugin' components to initialize.
Each element here needs to be the directory name of the plugin,
which is the final path component (without the \".git\" extension) of
the plugin's URI in `emacs-reveal-external-components'.
If you remove a plugin from this list, it will no longer be initialized.
If you add plugins to this list, you need to provide suitable
initialization code yourself.  (E.g., see the code concerning
\"reveal.js-plugins\" in emacs-reveal.el.)"
  :group 'emacs-reveal
  :type '(repeat string))

(defcustom emacs-reveal-latex-figure-float "htp"
  "Define position for floating figures in LaTeX export.
You may want to use \"H\" with the float package."
  :group 'emacs-reveal
  :type 'string)

;; The following options are only relevant if you use
;; reveal-export-image-grid to generate image grids.
;; Then, the options control in what directory generated CSS is saved.
(defcustom emacs-reveal-export-dir "public/"
  "Directory into which HTML, CSS, and Javascript is published.
The default supposes that `org-publish-all' publishes into a
subdirectory of `public/' as determined by
`emacs-reveal-css-filename-template'.  Note that the name must end
with a slash.  Also note that this directory is removed internally when
setting the option `REVEAL_EXTRA_CSS' (to create valid relative links
when everything is exported into this directory).
This is only used for CSS of image grids with
`emacs-reveal-export-image-grid'."
  :group 'emacs-reveal
  :type 'directory)

(defcustom emacs-reveal-css-filename-template
  "figures/internal_grid_css/grid%s.css"
  "Template for filename of CSS generated for image grid.
This must contain `%s' as placeholder for the grid's identifier.
Note that this filename is exported into a subdirectory of
`emacs-reveal-export-dir' under the current directory."
  :group 'emacs-reveal
  :type 'string)

(defconst emacs-reveal-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory of emacs-reveal containing code and resources.
Useful for `org-publish-all' to publish resources that are also
contained in this directory.")
(defconst emacs-reveal-install-prompt
  "Files for %s not found at: %s  Clone from source repository? ")
(defconst emacs-reveal-customize-msg
  "Make sure to configure your setup properly.")
(defconst emacs-reveal-plugin-not-used
  "Plugin %s will not be used.")

;; Functions to clone components:
(defun emacs-reveal--clone-component (component address branch)
  "Clone COMPONENT from ADDRESS and checkout BRANCH.
Target directory is `emacs-reveal-dir'."
  (shell-command (format "cd %s; git clone %s; cd %s; git checkout %s"
			 emacs-reveal-dir address component branch)))

(defun emacs-reveal-install ()
  "Install components listed in `emacs-reveal-external-components'.
It is questionable whether this function is useful.  Currently, such
components are included as Git submodules."
  (interactive)
  (dolist (triple emacs-reveal-external-components)
    (let* ((address (car triple))
	   (component (file-name-sans-extension
		       (car (last (split-string address "/")))))
	   (branch (cadr triple))
	   (type (cl-caddr triple))
	   (target-dir (expand-file-name component emacs-reveal-dir)))
      (unless (file-exists-p target-dir)
	(if (yes-or-no-p
	     (format emacs-reveal-install-prompt component target-dir))
	    (emacs-reveal--clone-component component address branch)
	  (cond ((eq type 'javascript)
		 (message emacs-reveal-customize-msg)
		 (sleep-for 2))
		(t
		 (message emacs-reveal-plugin-not-used component)
		 (sleep-for 2))))))))

;;; Configuration of various components.
(require 'org)
(require 'org-ref)
(require 'org-re-reveal)
(require 'org-re-reveal-ref)

;; Enable configurable loading of JavaScript libraries.  By default,
;; avoid loading of head.min.js, which does not exist any more.
(setq org-re-reveal-script-files emacs-reveal-script-files)

;; Fix URL fragments to use valid IDs.
(setq org-re-reveal--href-fragment-prefix org-re-reveal--slide-id-prefix)

;; Setup url package with hyphens option.  This is done here to avoid
;; option clashes when implicitly loading the package from hyperref.
(add-to-list 'org-latex-default-packages-alist
 	     (list "hyphens" "url" nil))

;; Load float package.  This must come before hyperref to avoid
;; warnings for figures: warning (ext4): destination with the same identifier
(add-to-list 'org-latex-default-packages-alist
 	     (list "" "float" nil))

;;; Configure reveal.js plugins.
;; Note that in the relative src-paths in org-re-reveal-external-plugins,
;; %s is automatically replaced with org-re-reveal-root.

;;(setq org-re-reveal-external-plugins nil)
(when (member "reveal.js-plugins" emacs-reveal-plugins)
  ;; Activate audio-slideshow plugin, but not multiple times when speaker
  ;; notes are shown.
  (add-to-list 'org-re-reveal-external-plugins
	       (cons 'audio-slideshow
		     " { src: '%splugin/audio-slideshow/audio-slideshow.js', condition: function( ) { return !!document.body.classList && !Reveal.isSpeakerNotes(); } }"))

  ;; Adjust audio-slideshow settings:
  ;; - Do not advance after end of audio
  ;; - Start playing audio automatically
  ;; - Do not display controls if no local audio file is given
  ;; - Increase opacity when unfocused (students found default too easy to miss)
  ;; - Display audio controls at bottom left (to avoid overlap)
  (setq org-re-reveal-init-script "  audio: {
    advance: -1, autoplay: true, defaultDuration: 0, playerOpacity: 0.3,
    playerStyle: 'position: fixed; bottom: 9.5vh; left: 0%; width: 30%; height:30px; z-index: 33;' }")

  ;; Activate anything plugin.
  (add-to-list 'org-re-reveal-external-plugins
	       (cons 'anything " { src: '%splugin/anything/anything.js' }"))
  (setq org-re-reveal-init-script
	(concat org-re-reveal-init-script
		",
  anything: [
	{className: \"animate\",  initialize: (function(container, options){
		Reveal.addEventListener( 'fragmentshown', function( event ) {
			if (typeof event.fragment.beginElement === \"function\" ) {
				event.fragment.beginElement();
			}
		});
		Reveal.addEventListener( 'fragmenthidden', function( event ) {
			if (event.fragment.hasAttribute('data-reverse') ) {
				var reverse = event.fragment.parentElement.querySelector('[id=\\\"' + event.fragment.getAttribute('data-reverse') + '\\\"]');
				if ( reverse && typeof reverse.beginElement === \"function\" ) {
					reverse.beginElement();
				}
			}
		});
		if ( container.getAttribute(\"data-svg-src\") ) {
			var xhr = new XMLHttpRequest();
			xhr.onload = function() {
				if (xhr.readyState === 4) {
					var svg = container.querySelector('svg');
					container.removeChild( svg );
					container.innerHTML = xhr.responseText + container.innerHTML;
					if ( svg ) {
						container.querySelector('svg').innerHTML = container.querySelector('svg').innerHTML + svg.innerHTML;
					}
				}
				else {
					console.warn( \"Failed to get file. ReadyState: \" + xhr.readyState + \", Status: \" + xhr.status);
				}
			};
			xhr.open( 'GET', container.getAttribute(\"data-svg-src\"), true );
			xhr.send();
		}
	}) },
	{className: \"randomPic\",
	 defaults: {imgalt: \"Dummy alt text\",
		    imgcaption: \"Image by {name}\",
		    choices: [ {name: \"dummyname\", path: \"dummypath\"} ]},
	 initialize: (function(container, options){
	     var choice = Math.trunc( Math.random()*(options.choices.length) );
	     var img = \"<img src='\" + options.choices[choice].path + \"' alt='\" + options.choices[choice].imgalt + \"' />\";
	     var caption = options.imgcaption.replace(new RegExp('\\{name\\}', 'gm'), options.choices[choice].name);
	     container.innerHTML = img + caption;
	 }) },
	{className: \"notes\",
	 initialize: (function(container, options){
	     container.addEventListener('click', function(e) { RevealNotes.open(); });
	 }) }
],")))

(when (member "Reveal.js-TOC-Progress" emacs-reveal-plugins)
  ;; Activate TOC progress plugin.
  ;; If there are lots of subsections, 'scroll'ing can be enabled or
  ;; the font size can be 'reduce'd.  Go for the latter.
  (add-to-list 'org-re-reveal-external-plugins
	       (cons 'toc-progress
		     " { src: '%splugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize('reduce', 'rgba(120,138,130,0.2)'); toc_progress.create(); } }")))

(when (member "reveal.js-jump-plugin" emacs-reveal-plugins)
  ;; Activate jump plugin.
  (add-to-list 'org-re-reveal-external-plugins
	       (cons 'jump "{ src: '%splugin/jump/jump.js', async: true }")))

(when (member "reveal.js-quiz" emacs-reveal-plugins)
  ;; Activate quiz plugin.
  (add-to-list 'org-re-reveal-external-plugins
	       (cons 'quiz "{ src: '%splugin/quiz/js/quiz.js', async: true, callback: function() { prepareQuizzes({preventUnanswered: true}); } }")))

;; Setup Bibliography in HTML based on default bib file (which helps to
;; locate the bib file when the current buffer does not specify one).
;; Display article, book, inproceedings differently.  Entry misc is new.
;; Remaining entries are defaults.
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

;;; Use (only) CSS to style tables, enable non-org tables.
(setq org-html-table-default-attributes nil)
(require 'table)
(setq table-html-th-rows 1
      table-html-table-attribute "class=\"emacs-table\"")

;;; Allow colored text.
;; The FAQ at http://orgmode.org/worg/org-faq.html contains a recipe
;; based on the obsolete function (since Org 9.0) org-add-link-type.
;; Adapted to use org-link-set-parameters:
(org-link-set-parameters
 "color"
 :follow (lambda (path)
	   (message (concat "color "
			    (progn (add-text-properties
				    0 (length path)
				    (list 'face `((t (:foreground ,path))))
				    path) path))))
 :export (lambda (path desc backend)
	   (cond
	    ((eq backend 'html)
	     (format "<span style=\"color:%s;\">%s</span>" path desc))
	    ((eq backend 'latex)
	     (format "{\\color{%s}%s}" path desc)))))

;;; Function to generate proper CC attribution for images.
;; Function emacs-reveal-export-attribution is used in macros in config.org.
;; See emacs-reveal-howto for sample use:
;; https://gitlab.com/oer/emacs-reveal-howto
(defun emacs-reveal-export-attribution (&rest args)
  "Generate HTML and LaTeX code for image with license attribution.
Essentially, this function calls `emacs-reveal--export-attribution-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2."
  ;; The first argument is the file name for metadata.  If that
  ;; starts with a quotation mark, arguments have been quoted.
  ;; (You don't start file names with quotation marks, do you?)
  (let ((metadata (car args)))
    (if (string-prefix-p "\"" metadata)
	(apply #'emacs-reveal--export-attribution-helper
	       (mapcar #'emacs-reveal--read-from-string args))
      (apply #'emacs-reveal--export-attribution-helper args))))

(defun emacs-reveal--check-symbol (object)
  "Helper function for `emacs-reveal--read-from-string'.
Different Org versions treat macro arguments differently.  Check whether
OBJECT is a quoted symbol, where no quoting is necessary.  Notify user
if applicable.  Raise `user-error' in case of unknown type."
  (if (and (consp object) (eq 'quote (car object)))
      (progn
	(message
	 "Explicit quoting of symbol in `%s' not necessary (with your Org version)"
	 object)
	(sit-for 2)
	(cadr object))
    (user-error "Unexpected type `%s' in `%s'" (type-of object) object)))

(defun emacs-reveal--read-from-string (object)
  "Undo potential quoting in OBJECT for strings with Org 9.2.
If OBJECT is a string, then use `read-from-string' to return
a boolean, integer, string, or symbol.
If OBJECT is not a string, return it unchanged."
  (if (stringp object)
      (if (= 0 (length object))
	  nil
	(let ((first (car (read-from-string object))))
	  (if (or (booleanp first) (integerp first) (stringp first)
		  (symbolp first))
	      first
	    (emacs-reveal--check-symbol first))))
    object))

(defun emacs-reveal--export-attribution-helper
    (metadata
     &optional caption maxheight divclasses shortlicense embed-svg)
  "Display image from METADATA.
Produce string for HTML and LaTeX exports to be embedded in Org files.
METADATA is a text file including licensing information.
If optional CAPTION is not nil, it can either be a string or t.  In that
case, display text underneath the image: If CAPTION is t, display whatever
the meta-data knows as title, otherwise display the string CAPTION, but
replace cite-links if present.
If CAPTION is nil, a LaTeX caption is generated anyways to have a numbered
figure (and frequently to also display license information).
Optional MAXHEIGHT restricts the height of the image and of the license
information in HTML.  MAXHEIGHT needs be a full specification including
the unit, e.g. `50vh'.
If present, optional DIVCLASSES must be a string with space separated
classes for the div element, including `figure'.
If optional SHORTLICENSE is the symbol `none', do not display license
text (useful if image license agrees with document license);
if it is t, display license based on `emacs-reveal--short-license-template'
\(instead of default (long) license text).
If optional EMBED-SVG is non-nil, embed XML code of SVG image directly.  In
this case, the maximum height on the image does not have any effect.
For LaTeX, the METADATA file may specify a texwidth, which is embedded in
the width specification as fraction of `linewidth'; 0.9 by default."
  (let ((org (emacs-reveal--attribution-strings
	      metadata caption maxheight divclasses shortlicense embed-svg)))
    (concat (if caption
		(concat "@@html: </p><div class=\"imgcontainer\">"
			(car org)
			"</div><p>@@")
	      (concat "@@html: </p>" (car org) "<p>@@"))
	    "\n"
	    (cdr org))))

(defvar emacs-reveal--short-license-template "[[%s][Figure]] under [[%s][%s]]")
(defvar emacs-reveal--figure-div-template  "<div about=\"%s\" class=\"%s\"><p><img src=\"%s\" alt=\"%s\" %s/></p>%s%s</div>")
(defvar emacs-reveal--svg-div-template  "<div about=\"%s\" class=\"%s\"><p>%s</p>%s%s</div>")
(defvar emacs-reveal--figure-latex-caption-template "#+BEGIN_EXPORT latex\n\\begin{figure}[%s] \\centering\n  \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s (%s)}\n  \\end{figure}\n#+END_EXPORT\n")
(defvar emacs-reveal--figure-latex-template "         #+BEGIN_EXPORT latex\n     \\begin{figure}[%s] \\centering\n       \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s}\n     \\end{figure}\n         #+END_EXPORT\n")
(defvar emacs-reveal--figure-external-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} External figure \\textbf{not} included: %s \\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defvar emacs-reveal--figure-unsupported-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} Figure omitted as %s format \\textbf{not} supported in \\LaTeX: “%s”\\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defvar emacs-reveal--unsupported-tex-figure-formats '("gif"))

;; Image grid variables
(defvar emacs-reveal--css-grid-img-class-template "grid%s-img%d"
  "Template for name of grid class.")
(defvar emacs-reveal--css-grid-img-template
  (concat "." emacs-reveal--css-grid-img-class-template
	  " { grid-area: ga%d; }")
  "Template for CSS of img element.")
(defvar emacs-reveal--css-repeat-template "repeat(%s, 1fr)"
  "Template for size of rows and columns.")
(defvar emacs-reveal--css-grid-template ".grid%s {
  display: grid;
  height: %svh;
  grid-template-columns: %s;
  grid-template-rows: %s;
  grid-gap: 5px;
  align-items: center;
  grid-template-areas: %s; }
"
  "Template for CSS of grid.")
(defvar emacs-reveal--css-grid-img-all ".grid-img img {
  max-width: 90%; }
"
  "CSS for all images of grid.")

(defun emacs-reveal--export-figure-latex
    (filename texwidth texfilename texlicense &optional latexcaption)
  "Generate LaTeX for figure at FILENAME.
If FILENAME is a full HTTP(S) URL, use
`emacs-reveal--figure-external-latex-template' as placeholder.
If FILENAME has an unsupported extension (included in
`emacs-reveal--unsupported-tex-figure-formats'), use
`emacs-reveal--figure-unsupported-latex-template' as placeholder.
Otherwise, include graphics at TEXFILENAME of width TEXWIDTH
with caption TEXLICENSE.  Optional LATEXCAPTION determines whether
`emacs-reveal--figure-latex-template' or
`emacs-reveal--figure-latex-caption-template' is used to generate LaTeX code."
  (cond ((string-match-p "^https?://" filename)
	 (format emacs-reveal--figure-external-latex-template texlicense))
	((member (file-name-extension filename)
		 emacs-reveal--unsupported-tex-figure-formats)
	 (format emacs-reveal--figure-unsupported-latex-template
		 (file-name-extension filename) texlicense))
	(latexcaption
	 (format emacs-reveal--figure-latex-caption-template
		 emacs-reveal-latex-figure-float
		 texwidth texfilename latexcaption texlicense))
	(t (format emacs-reveal--figure-latex-template
		   emacs-reveal-latex-figure-float
		   texwidth texfilename texlicense))))

(defun emacs-reveal--export-figure-html
    (filename divclasses htmlcaption htmllicense imgalt h-image
	      &optional embed-svg)
  "Generate HTML for figure at FILENAME.
DIVCLASSES is passed from `emacs-reveal-export-attribution',
HTMLCAPTION and HTMLLICENSE caption and license information for
the figure in HTML format.
If optional EMBED-SVG is non-nil, the file must be an SVG image
which is embedded directly.  SVG images are also embedded directly if
single file export is requested, which fails if a H-IMAGE is given.
Otherwise, an img tag is used, for which optional parameter IMGALT provides
the text for the alt attribute, while H-IMAGE specifies the height of the
image.
Templates `emacs-reveal--svg-div-template' and
`emacs-reveal--figure-div-template'specify the general HTML format."
  (let* ((extension (file-name-extension filename))
	 (external (string-match-p "^https?://" filename))
	 (issvg (and (string= "svg" extension) (not external)))
	 (issingle (plist-get (org-export-get-environment 're-reveal)
			      :reveal-single-file)))
    (if (and issvg issingle (not embed-svg))
	(user-error "Cannot produce single file without embedding SVG: %s"
		    filename)
      (if embed-svg
	  ;; Embed SVG's XML directly.
	  (format emacs-reveal--svg-div-template
		  filename divclasses
		  (emacs-reveal--file-as-string filename t)
		  htmlcaption htmllicense)
	(format emacs-reveal--figure-div-template
		filename divclasses
		(if (and issingle (not external))
		    ;; Insert base64 encoded image as single line.
		    (concat "data:image/" extension ";base64,"
			    (with-temp-buffer
			      (insert-file-contents-literally filename)
			      (base64-encode-region 1 (point-max) t)
			      (buffer-string)))
		  filename)
		imgalt h-image htmlcaption htmllicense)))))

(defun emacs-reveal--export-no-newline (string backend)
  "Call `org-export-string-as' on STRING, BACKEND, and t;
remove newline characters and, in case of HTML, surrounding p tags,
and return as result."
  (replace-regexp-in-string "\n\\|<p>\\|</p>" " "
			    (org-export-string-as string backend t)))

(defun emacs-reveal--file-as-string (filename &optional no-newlines)
  "Return contents of FILENAME as string.
If optional NO-NEWLINES is non-nil, return result without newlines."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (if no-newlines
	(replace-regexp-in-string
	 "\n" " " (buffer-substring-no-properties (point-min) (point-max)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun emacs-reveal--attribution-strings
    (metadata &optional caption maxheight divclasses shortlicense embed-svg)
  "Helper function.
See `emacs-reveal-export-attribution' and
`emacs-reveal--export-attribution-helper' for description of arguments
CAPTION, MAXHEIGHT, DIVCLASSES, SHORTLICENSE, EMBED-SVG.
Return cons cell whose car is the HTML representation for METADATA
and whose cdr is the LaTeX representation."
  (let* ((org-export-with-sub-superscripts nil)
	 (alist (read (emacs-reveal--file-as-string metadata)))
	 (filename (alist-get 'filename alist))
	 (texfilename (file-name-sans-extension filename))
	 (licenseurl (alist-get 'licenseurl alist))
	 (licensetext (alist-get 'licensetext alist))
	 (permit (if (alist-get 'permit alist)
		     (format ". %s" (alist-get 'permit alist))
		   ""))
	 (attributionname (alist-get 'cc:attributionName alist))
	 (attributionurl (alist-get 'cc:attributionURL alist))
	 (orgauthor (if attributionname
			(if attributionurl
			    (format "by [[%s][%s]]"
				    attributionurl attributionname)
			  (format "by %s" attributionname))
		      ""))
	 (htmlauthor (if attributionname
			 (if attributionurl
			     (format "by <a rel=\"dc:creator\" href=\"%s\" property=\"cc:attributionName\">%s</a>"
				     attributionurl attributionname)
			   (format "by <span property=\"dc:creator cc:attributionName\">%s</span>"
				   attributionname))
		       ""))
	 (title (alist-get 'dc:title alist "Image"))
	 (realcaption (when caption
			(if (stringp caption)
			    caption
			  title)))
	 (htmlcaption (format "<p>%s</p>"
			      (if realcaption
				  (emacs-reveal--export-no-newline realcaption 'html)
				"")))
	 (latexcaption (when realcaption
			 (emacs-reveal--export-no-newline realcaption 'latex)))
	 (htmltitle (format "<span property=\"dc:title\">%s</span>"
			    (emacs-reveal--export-no-newline title 'html)))
	 (imgalt (or (alist-get 'imgalt alist)
		     title))
	 (imgadapted (alist-get 'imgadapted alist "from"))
	 (sourceuri (alist-get 'dc:source alist))
	 (sourcetext (alist-get 'sourcetext alist))
	 (sourcehtml (format "; %s <a rel=\"dc:source\" href=\"%s\">%s</a>"
			     imgadapted sourceuri sourcetext))
	 (divclasses (if divclasses
			 divclasses
		       "figure"))
	 (texwidth (alist-get 'texwidth alist 0.9))
	 (h-image (if maxheight
		      (format " style=\"max-height:%s\"" maxheight)
		    ""))
	 (h-license (if maxheight
			(format " style=\"max-width:%s\"" maxheight)
		      ""))
	 (orglicense (cond ((eq shortlicense 'none) "")
			   (shortlicense (format
					  emacs-reveal--short-license-template
					  sourceuri licenseurl licensetext))
			   (t (format "“%s” %s under [[%s][%s]]; %s [[%s][%s]]%s"
				      title orgauthor licenseurl licensetext
				      imgadapted sourceuri sourcetext permit))))
	 (htmllicense (cond ((eq shortlicense 'none) "")
			    (shortlicense (format
					   "<p%s>%s</p>" h-license
					   (emacs-reveal--export-no-newline
					    orglicense 'html)))
			    (t (format
				"<p%s>&ldquo;%s&rdquo; %s under <a rel=\"license\" href=\"%s\">%s</a>%s%s</p>"
				h-license htmltitle htmlauthor licenseurl
				licensetext sourcehtml
				(emacs-reveal--export-no-newline permit 'html)))))
	 (texlicense (if (< 0 (length orglicense))
			 (emacs-reveal--export-no-newline orglicense 'latex)
		       (emacs-reveal--export-no-newline title 'latex)))
	 )
    (if (stringp caption)
	(cons (emacs-reveal--export-figure-html
	       filename divclasses htmlcaption htmllicense imgalt h-image
	       embed-svg)
	      (emacs-reveal--export-figure-latex
	       filename texwidth texfilename texlicense latexcaption))
      (cons (emacs-reveal--export-figure-html
	     filename divclasses
	     ;; In general, the title is part of the license text, and
	     ;; we do not display it twice.
	     ;; If a short license is requested, the title is not part
	     ;; of the license but passed here.
	     (if shortlicense htmlcaption "<p></p>")
	     htmllicense imgalt h-image embed-svg)
	    (emacs-reveal--export-figure-latex
	     filename texwidth texfilename texlicense
	     ;; Similar to above case.  However, a LaTeX caption is always
	     ;; generated via texlicense.
	     ;; Only use latexcaption when shortlicense is t
	     ;; (but not if it is none).
	     (when (and shortlicense (booleanp shortlicense))
	       latexcaption))))))

;;; Function to create a grid of images with license information in HTML.
;; Function emacs-reveal-export-image-grid is used in macro in config.org.
;; See emacs-reveal-howto for sample use:
;; https://gitlab.com/oer/emacs-reveal-howto
(defun emacs-reveal-export-image-grid (&rest args)
  "Generate HTML for image grid.
Essentially, this function calls `emacs-reveal--export-image-grid-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2."
  ;; The first argument is an integer ID.  If that is a string,
  ;; arguments have been quoted.
  (if (stringp (car args))
      (apply #'emacs-reveal--export-image-grid-helper
	     (mapcar #'emacs-reveal--read-from-string args))
    (apply #'emacs-reveal--export-image-grid-helper args)))

(defun emacs-reveal--export-image-grid-helper
    (grid-id grid-images height no-columns no-rows template-areas)
  "Create HTML to display grid with id GRID-ID of GRID-IMAGES.
The grid has a HEIGHT (percentage of viewport height without unit),
NO-COLUMNS columns, NO-ROWS rows; positioning is specified by TEMPLATE-AREAS."
  (let* ((images (read (emacs-reveal--file-as-string grid-images)))
	 (no-images (length images))
	 (numbered (cl-mapcar #'cons (number-sequence 1 no-images) images))
	 (row-height (/ (* 0.95 height) no-rows))
	 (image-heights (emacs-reveal--compute-image-heights template-areas)))
    (emacs-reveal--save-image-grid-css
     grid-id images height no-columns no-rows template-areas)
    (concat (format "#+REVEAL_EXTRA_CSS: %s\n"
		    (format emacs-reveal-css-filename-template grid-id))
	    (format "@@html: </p><div class=\"grid%s\">" grid-id)
	    (mapconcat (lambda (pair)
			 (emacs-reveal--export-grid-image
			  grid-id row-height image-heights
			  (car pair) (cdr pair)))
		       numbered " ")
	    "</div><p>@@"
	    "\n"
	    "@@latex: Presentation contains image grid.  \\LaTeX export not supported.@@")))

(defun emacs-reveal--generate-grid-img (grid-id no)
  "Create CSS class assigning grid-area NO to image NO in grid GRID-ID."
  (format emacs-reveal--css-grid-img-template grid-id no no))

(defun emacs-reveal--generate-grid-imgs (grid-id no-images)
  "Create CSS classes for GRID-ID assigning grid areas for NO-IMAGES images."
  (mapconcat (lambda (no) (emacs-reveal--generate-grid-img grid-id no))
	     (number-sequence 1 no-images) "\n"))

(defun emacs-reveal--generate-grid
    (grid-id height no-columns no-rows template-areas)
  "Create CSS for grid layout of GRID-ID.
Layout based on `emacs-reveal--css-grid-template' requires HEIGHT,
NO-COLUMNS, NO-ROWS, TEMPLATE-AREAS."
  (format emacs-reveal--css-grid-template grid-id height
	  (format emacs-reveal--css-repeat-template no-columns)
	  (format emacs-reveal--css-repeat-template no-rows)
	  template-areas))

(defun emacs-reveal--save-image-grid-css
    (grid-id images height no-columns no-rows template-areas)
  "Save CSS for GRID-ID with IMAGES to file.
Helper function for `emacs-reveal-export-image-grid', see there for
documentation of arguments HEIGHT, NO-COLUMNS, NO-ROWS, TEMPLATE-AREAS.
Construct name of file in `emacs-reveal-export-dir' with
`emacs-reveal-css-filename-template', create directories if necessary,
remove possibly previously existing file, write CSS to new file, and
return it's name."
  (let* ((no-images  (length images))
	 (filename (expand-file-name
		    (format emacs-reveal-css-filename-template grid-id)
		    emacs-reveal-export-dir))
	 (dirname (file-name-directory filename))
	 (css (concat (emacs-reveal--generate-grid-imgs grid-id no-images)
		      "\n"
		      (emacs-reveal--generate-grid
		       grid-id height no-columns no-rows template-areas)
		      emacs-reveal--css-grid-img-all "\n")))
    (mkdir dirname t)
    (when (file-readable-p filename)
      (delete-file filename))
    (append-to-file css nil filename)
    filename))

(defun emacs-reveal--compute-image-heights (template-areas)
  "Create hash table with heights of cells in TEMPLATE-AREAS."
  (let ((rows (delete "" (delete " " (split-string template-areas "\""))))
	(result (make-hash-table :test 'equal)))
    (dolist (row rows result)
      (let ((cells (delete-dups (split-string row " "))))
	(dolist (cell cells)
	  (puthash cell (+ 1 (gethash cell result 0)) result))))))

(defun emacs-reveal--export-grid-image
    (grid-id row-height image-heights no image)
  "Create HTML for IMAGE number NO in GRID-ID.
The height of the row is ROW-HEIGHT, heights of images are given by
IMAGE-HEIGHTS.
Call `emacs-reveal--attribution-strings' with proper metadata."
  (let ((area (format "ga%d" no)))
    (car (emacs-reveal--attribution-strings
	  image nil
	  (format "%svh"
		  (* (gethash area image-heights) row-height))
	  (concat "figure grid-img "
		  (format emacs-reveal--css-grid-img-class-template
			  grid-id no))))))

;;; Functionality to make org-html-link use org-re-reveal's ID format.
;; This is useful when publishing with org-html-publish-to-html
;; where the HTML file is supposed to link into presentations.
;; Sample use: https://gitlab.com/oer/OS/blob/master/elisp/publish.el
(defun emacs-reveal--rewrite-link (old-fun &rest arguments)
  "Combine OLD-FUN on ARGUMENTS with `org-re-reveal--maybe-replace-in-link'."
  (let ((orig (apply old-fun arguments)))
    (org-re-reveal--maybe-replace-in-link orig t)))

(defun emacs-reveal--add-advice-link (&rest arguments)
  "Extend `org-html-link' with advice for org-re-reveal's anchor ID format.
ARGUMENTS are unused (but present to allow invocation as preparation
function during Org export, which passes an argument)."
  (ignore arguments) ; Silence byte compiler
  (advice-add #'org-html-link :around #'emacs-reveal--rewrite-link))

(defun emacs-reveal--remove-advice-link (&rest arguments)
  "Remove advice on `org-html-link'.
ARGUMENTS are unused (but present to allow invocation as completion
function during Org export, which passes an argument)."
  (ignore arguments) ; Silence byte compiler
  (advice-remove #'org-html-link #'emacs-reveal--rewrite-link))

;;; Extract version string.
(defun emacs-reveal-version ()
  "Display version string for emacs-reveal from Lisp file."
  (interactive)
  (let ((lisp-file
	 (concat (file-name-sans-extension (locate-library "emacs-reveal"))
		 ".el")))
    (with-temp-buffer
      (insert-file-contents lisp-file)
      (goto-char (point-min))
      (re-search-forward "^;; Version: \\([0-9.]+\\)$")
      (message "emacs-reveal version %s" (match-string 1)))))

(provide 'emacs-reveal)
;;; emacs-reveal.el ends here
