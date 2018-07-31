;;; reveal-config.el --- Configuration for org-reveal in GNU Emacs
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2017, 2018 Jens Lechtenbörger

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
;; This file contains configuration options to create reveal.js
;; presentations from org mode files in GNU Emacs with org-reveal
;; and some reveal.js plugins.
;; Load this as follows from ~/.emacs: (load "path/to/reveal-config.el")

;;; Code:
(add-to-list 'load-path (expand-file-name
			 "./org-reveal" (file-name-directory load-file-name)))
(require 'ox-reveal)

;;; Configure reveal.js plugins.
;; Note that in the relative src-paths in org-reveal-external-plugins,
;; %s is automatically replaced with org-reveal-root.

;;(setq org-reveal-external-plugins nil)
;; Activate audio-slideshow plugin, but not multiple times when speaker
;; notes are shown.
(add-to-list 'org-reveal-external-plugins
	     (cons 'audio-slideshow
		   " { src: '%splugin/audio-slideshow/audio-slideshow.js', condition: function( ) { return !!document.body.classList && !Reveal.isSpeakerNotes(); } }"))
;; Adjust audio-slideshow settings:
;; - Do not advance after end of audio
;; - Start playing audio automatically
;; - Do not display controls if no local audio file is given
;; - Increase opacity when unfocused (students found default too easy to miss)
;; - Display audio controls at bottom left (to avoid overlap)
(setq org-reveal-init-script "  audio: {
    advance: -1, autoplay: true, defaultDuration: 0, playerOpacity: 0.3,
    playerStyle: 'position: fixed; bottom: 9.5vh; left: 0%; width: 30%; height:30px; z-index: 33;' }")

;; Activate anything plugin
(add-to-list 'org-reveal-external-plugins
	     (cons 'anything " { src: '%splugin/anything/anything.js' }"))
(setq org-reveal-init-script
      (concat org-reveal-init-script
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
],"))

;; Activate TOC progress plugin
;; If there are lots of subsections, 'scroll'ing can be enabled or
;; the font size can be 'reduce'd.  Go for the latter.
(add-to-list 'org-reveal-external-plugins
	     (cons 'toc-progress
		   " { src: '%splugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize('reduce', 'rgba(120,138,130,0.2)'); toc_progress.create(); } }"))

;; Activate jump plugin
(add-to-list 'org-reveal-external-plugins
	     (cons 'jump "{ src: '%splugin/jump/jump.js', async: true }"))

;; Activate quiz plugin
(add-to-list 'org-reveal-external-plugins
       (cons 'quiz "{ src: '%splugin/quiz/js/quiz.js', async: true, callback: function() { prepareQuizzes({preventUnanswered: true}); } }"))

;;; Use org-ref to enable citations.
(require 'org-ref)
;; If the declaration of the bibliography file is part of an included file,
;; org-ref does not know about it.  Use a default bibliography then.
(setq org-ref-default-bibliography "references.bib")

;; With org-reveal, only slide titles can be link targets.
;; Setup Bibliography in HTML.
;; This is a dirty hack.  A paragraph is opened before the HTML below
;; is generated.  Thus, the paragraph must be closed first.  For valid
;; HTML, the org file must contain "@@html:<p>@@" after the bibliography
;; command.
(setq org-ref-bib-html "</p><div class=\"slide-footer\"><br></div></section>
<section id=\"slide-bibliography\" data-state=\"no-toc-progress\">
<h3 class=\"org-ref-bib-h3 no-toc-progress\">Bibliography</h3>\n")
(setq org-ref-ref-html
      "<a class=\"org-ref-reference\" href=\"#/slide-bibliography\">[%s]</a>")

;; Display article, book, inproceedings, misc differently.
;; Remaining entries are defaults.
(setq org-ref-bibliography-entry-format
      '(("article" . "%a, %t, <i>%j %v(%n)</i>, %p (%y). <a href=\"%U\">%U</a>")
	("book" . "%a, %t, %u, %y. <a href=\"%U\">%U</a>")
	("incollection" . "%a, %t, %b, %u, %y. <a href=\"%U\">%U</a>")
	("inproceedings" . "%a, %t, %b, %y. <a href=\"%U\">%U</a>")
	("misc" . "%a, %t, %i, %y.  <a href=\"%U\">%U</a>")
	("techreport" . "%a, %t, %i, %u (%y).")
	("proceedings" . "%e, %t in %S, %u (%y).")
	))

;; Use (only) CSS to style tables, enable non-org tables
(setq org-html-table-default-attributes nil)
(require 'table)
(setq table-html-th-rows 1
      table-html-table-attribute "class=\"emacs-table\"")

;; Allow colored text.
;; Following copied from the FAQ: http://orgmode.org/worg/org-faq.html
(org-add-link-type
 "color"
 (lambda (path)
   (message (concat "color "
                    (progn (add-text-properties
                            0 (length path)
                            (list 'face `((t (:foreground ,path))))
                            path) path))))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))

;; Function to generate proper CC attribution for images.  The function
;; is used in macros in config.org.
;; See emacs-reveal-howto for sample use:
;; https://gitlab.com/oer/emacs-reveal-howto
(defun reveal-export-attribution
    (metadata &optional caption maxheight divclasses shortlicense)
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
if it is t, display license based on `reveal--short-license-template'
\(instead of default (long) license text).
For LaTeX, the METADATA file may specify a texwidth, which is embedded in
the width specification as fraction of `linewidth'; 0.9 by default."
  (let ((org (reveal--attribution-strings
	      metadata caption maxheight divclasses shortlicense)))
    (concat (if caption
		(concat "@@html: </p><div class=\"imgcontainer\">"
			(car org)
			"</div><p>@@")
	      (concat "@@html: </p>" (car org) "<p>@@"))
	    "\n"
	    (cdr org))))

(defvar reveal--short-license-template "[[%s][Figure]] under [[%s][%s]]")
(defvar reveal--figure-div-template  "<div about=\"%s\" class=\"%s\"><p><img src=\"%s\" alt=\"%s\" %s/></p>%s%s</div>")
(defvar reveal--svg-div-template  "<div about=\"%s\" class=\"%s\"><p>%s</p>%s%s</div>")
(defvar reveal--figure-latex-caption-template "#+BEGIN_EXPORT latex\n\\begin{figure}[htp] \\centering\n  \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s (%s)}\n  \\end{figure}\n#+END_EXPORT\n")
(defvar reveal--figure-latex-template "         #+BEGIN_EXPORT latex\n     \\begin{figure}[htp] \\centering\n       \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s}\n     \\end{figure}\n         #+END_EXPORT\n")
(defvar reveal--figure-external-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} External figure \\textbf{not} included: %s \\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defvar reveal--figure-unsupported-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} Figure omitted as %s format \\textbf{not} supported in \\LaTeX: “%s”\\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defvar reveal--unsupported-tex-figure-formats '("gif"))

(defun reveal--export-figure-latex (filename texwidth texfilename texlicense
					     &optional latexcaption)
  "Generate LaTeX for figure at FILENAME.
If FILENAME is a full HTTP(S) URL, use
`reveal--figure-external-latex-template' as placeholder.
If FILENAME has an unsupported extension (included in
`reveal--unsupported-tex-figure-formats'), use
`reveal--figure-unsupported-latex-template' as placeholder.
Otherwise, include graphics at TEXFILENAME of width TEXWIDTH
with caption TEXLICENSE.  Optional LATEXCAPTION determines whether
`reveal--figure-latex-template' or
`reveal--figure-latex-caption-template' is used to generate LaTeX code."
  (cond ((string-match-p "^https?://" filename)
	 (format reveal--figure-external-latex-template texlicense))
	((member (file-name-extension filename)
		 reveal--unsupported-tex-figure-formats)
	 (format reveal--figure-unsupported-latex-template
		 (file-name-extension filename) texlicense))
	(latexcaption
	 (format reveal--figure-latex-caption-template
		 texwidth texfilename latexcaption texlicense))
	(t (format reveal--figure-latex-template
	      texwidth texfilename texlicense))))

(defun reveal--export-figure-html
    (filename divclasses htmlcaption htmllicense imgalt h-image)
  "Generate HTML for figure at FILENAME.
DIVCLASSES is passed from `reveal-export-attribution',
HTMLCAPTION and HTMLLICENSE caption and license information for
the figure in HTML format.
If the file is a local SVG image, it is embedded directly; otherwise,
an img tag is used, for which optional parameter IMGALT provides
the text for the alt attribute, while H-IMAGE specifies the height
of the image.
Templates `reveal--svg-div-template' and `reveal--figure-div-template'
specify the general HTML format."
  (let ((issvg (and (string= "svg" (file-name-extension filename))
		    (not (string-match-p "^https?://" filename)))))
    (if issvg
	(format reveal--svg-div-template
		filename divclasses
		(reveal--file-as-string filename t)
		htmlcaption htmllicense)
      (format reveal--figure-div-template
	      filename divclasses filename imgalt h-image
	      htmlcaption htmllicense))))

(defun reveal--export-no-newline (string backend)
  "Call `org-export-string-as' on STRING, BACKEND, and t;
remove newline characters and, in case of HTML, surrounding p tags,
and return as result."
  (replace-regexp-in-string "\n\\|<p>\\|</p>" " "
			    (org-export-string-as string backend t)))

(defun reveal--file-as-string (filename &optional no-newlines)
  "Return contents of FILENAME as string.
If optional NO-NEWLINES is non-nil, return result without newlines."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (if no-newlines
	(replace-regexp-in-string
	 "\n" " " (buffer-substring-no-properties (point-min) (point-max)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun reveal--attribution-strings
    (metadata &optional caption maxheight divclasses shortlicense)
  "Helper function.
See `reveal-export-attribution' for description of arguments.
Return cons cell whose car is the HTML representation for METADATA
and whose cdr is the LaTeX representation."
  (let* ((org-export-with-sub-superscripts nil)
	 (alist (read (reveal--file-as-string metadata)))
	 (filename (alist-get 'filename alist))
	 (issvg (and (string= "svg" (file-name-extension filename))
		     (not (string-match-p "^https?://" filename))))
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
				  (reveal--export-no-newline realcaption 'html)
				"")))
	 (latexcaption (when realcaption
			 (reveal--export-no-newline realcaption 'latex)))
	 (htmltitle (format "<span property=\"dc:title\">%s</span>"
			    (reveal--export-no-newline title 'html)))
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
					  reveal--short-license-template
					  sourceuri licenseurl licensetext))
			   (t (format "“%s” %s under [[%s][%s]]; %s [[%s][%s]]"
				      title orgauthor licenseurl licensetext
				      imgadapted sourceuri sourcetext))))
	 (htmllicense (cond ((eq shortlicense 'none) "")
			    (shortlicense (format
					   "<p%s>%s</p>" h-license
					   (reveal--export-no-newline
					    orglicense 'html)))
			    (t (format
				"<p%s>&ldquo;%s&rdquo; %s under <a rel=\"license\" href=\"%s\">%s</a>%s%s</p>"
				h-license htmltitle htmlauthor licenseurl
				licensetext sourcehtml permit))))
	 (texlicense (if (< 0 (length orglicense))
			 (reveal--export-no-newline orglicense 'latex)
		       (reveal--export-no-newline title 'latex)))
	 )
    (if (stringp caption)
	(cons (reveal--export-figure-html
	       filename divclasses htmlcaption htmllicense imgalt h-image)
	      (reveal--export-figure-latex
	       filename texwidth texfilename texlicense latexcaption))
      (cons (reveal--export-figure-html
	     filename divclasses
	     ; In general, the title is part of the license text, and
	     ; we do not display it twice.
	     ; If a short license is requested, the title is not part
	     ; of the license but passed here.
	     (if shortlicense htmlcaption "<p></p>")
	     htmllicense imgalt h-image)
	    (reveal--export-figure-latex
	     filename texwidth texfilename texlicense
	     ; Similar to above case.  However, a LaTeX caption is always
	     ; generated via texlicense.
	     ; Only use latexcaption when shortlicense is t
	     ; (but not if it is none).
	     (when (and shortlicense (booleanp shortlicense))
	       latexcaption))))))

;; Function to create a grid of images with license information in HTML.
(defun reveal-export-image-grid
    (grid-id grid-images height no-columns no-rows template-areas)
  "Create HTML to display grid with id GRID-ID of GRID-IMAGES.
The grid has a HEIGHT (percentage of viewport height without unit),
NO-COLUMNS columns, NO-ROWS rows; positioning is specified by TEMPLATE-AREAS."
  (let* ((images (read (reveal--file-as-string grid-images)))
	 (reveal--internal-grid-id grid-id)
	 (reveal--internal-grid-img-counter 0)
	 (reveal--internal-grid-row-height (/ (* 0.95 height) no-rows))
	 (reveal--internal-image-heights
	  (reveal--compute-image-heights template-areas))
	 (css-name (reveal--save-image-grid-css
		    grid-id images height no-columns no-rows template-areas)))
    (concat (format "#+REVEAL_EXTRA_CSS: %s\n"
		    (replace-regexp-in-string "/public" "" css-name))
	    (format "@@html: </p><div class=\"grid%s\">" grid-id)
	    (mapconcat 'reveal--export-grid-image images " ")
	    "</div><p>@@"
	    "\n"
	    "@@latex: Presentation contains image grid.  \\LaTeX export not supported.@@")))

(defvar reveal--internal-grid-id 0
  "Unqiue integer number of grid.")
(defvar reveal--internal-grid-row-height 0
  "Computed from height of grid and number of rows.")
(defvar reveal--internal-image-heights 0
  "Height of image in number of cells.")
(defvar reveal--css-filename-template "./public/figures/css/grid%s.css"
  "Template for filename of grid's exported CSS.")
(defvar reveal--css-grid-img-class-template "grid%s-img%d"
  "Template for name of grid class.")
(defvar reveal--css-grid-img-template
  (concat "." reveal--css-grid-img-class-template "{ grid-area: ga%d; }")
  "Template for CSS of img element.")
(defvar reveal--css-repeat-template "repeat(%s, 1fr)"
  "Template for size of rows and columns.")
(defvar reveal--css-grid-template ".grid%s {
  display: grid;
  height: %svh;
  grid-template-columns: %s;
  grid-template-rows: %s;
  grid-gap: 5px;
  align-items: center;
  grid-template-areas: %s; }
"
  "Template for CSS of grid.")
(defvar reveal--css-grid-img-all ".grid-img img {
  max-width: 90%; }
"
  "CSS for all images of grid.")

(defun reveal--generate-grid-img (no)
  "Create CSS class assigning grid-area NO to image NO in current grid."
  (format reveal--css-grid-img-template
	  reveal--internal-grid-id no no))

(defun reveal--generate-grid-imgs (no-images)
  "Create CSS classes assigning grid areas for NO-IMAGES images.
Use id of current grid according to `reveal--internal-grid-id'."
  (mapconcat 'reveal--generate-grid-img
	     (number-sequence 1 no-images) "\n"))

(defun reveal--generate-grid
    (height no-columns no-rows template-areas)
  "Create CSS for grid layout based on `reveal--css-grid-template'."
  (format reveal--css-grid-template
	  reveal--internal-grid-id height
	  (format reveal--css-repeat-template no-columns)
	  (format reveal--css-repeat-template no-rows)
	  template-areas))

(defun reveal--save-image-grid-css
    (grid-id images height no-columns no-rows template-areas)
  "Save CSS for GRID-ID to file.
Helper function for `reveal-export-image-grid', see there for documentation
of further arguments.
Construct name of file with `reveal--css-filename-template', create
directories if necessary, remove possibly previously existing file,
write CSS to new file, and return it's name."
  (let* ((reveal--internal-grid-id grid-id)
	 (no-images  (length images))
	 (filename (format reveal--css-filename-template grid-id))
	 (dirname (file-name-directory filename))
	 (css (concat (reveal--generate-grid-imgs no-images)
		      "\n"
		      (reveal--generate-grid
		       height no-columns no-rows template-areas)
		      reveal--css-grid-img-all "\n")))
    (mkdir dirname t)
    (when (file-readable-p filename)
      (delete-file filename))
    (append-to-file css nil filename)
    filename))

(defun reveal--compute-image-heights (template-areas)
  "Create hash table with heights of cells in TEMPLATE-AREAS."
  (let ((rows (delete "" (delete " " (split-string template-areas "\""))))
	(result (make-hash-table :test 'equal)))
    (dolist (row rows result)
      (let ((cells (delete-dups (split-string row " "))))
	(dolist (cell cells)
	  (puthash cell (+ 1 (gethash cell result 0)) result))))))

(defun reveal--export-grid-image (image)
  "Create HTML for IMAGE.
Call `reveal--attribution-strings' with proper metadata."
  (setq reveal--internal-grid-img-counter
	(+ 1 reveal--internal-grid-img-counter))
  (let ((area (format "ga%d" reveal--internal-grid-img-counter)))
    (car (reveal--attribution-strings
	  image nil
	  (format "%svh"
		  (* (gethash area reveal--internal-image-heights)
		     reveal--internal-grid-row-height))
	  (concat "figure grid-img "
		  (format reveal--css-grid-img-class-template
			  reveal--internal-grid-id
			  reveal--internal-grid-img-counter))))))

(provide 'reveal-config)
;;; reveal-config.el ends here
