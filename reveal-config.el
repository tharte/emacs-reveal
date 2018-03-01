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
    playerStyle: 'position: fixed; bottom: 40px; left: 0%; width: 30%; height:30px; z-index: 33;' }")

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
;; For multiple CSS files, separate them with "\n" in org-reveal-extra-css:
(setq org-reveal-extra-css
      (concat "./reveal.js/plugin/toc-progress/toc-progress.css"
	      "\n"
	      "./reveal.js/css/theme/toc-style.css"))

;; Activate jump plugin
(add-to-list 'org-reveal-external-plugins
	     (cons 'jump "{ src: '%splugin/jump/jump.js', async: true }"))

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
(defun reveal-export-attribution (metadata &optional caption)
  "Generate string to represent image from METADATA with CAPTION.
Produce string for HTML and LaTeX exports to be embedded in Org files.
METADATA is a text file including licensing information.
If optional CAPTION is nil, just construct #+CAPTION option, which is
misused to display the license information.
Otherwise, generate full image with caption and license information for
HTML and LaTeX."
  (let* ((org-export-with-sub-superscripts nil)
	 (alist
	  (read (with-temp-buffer
		  (insert-file-contents-literally metadata)
		  (buffer-substring-no-properties (point-min) (point-max)))))
	 (filename (alist-get 'filename alist))
	 (licenseurl (alist-get 'licenseurl alist))
	 (licensetext (alist-get 'licensetext alist))
	 (attributionname (alist-get 'cc:attributionName alist))
	 (attributionurl (alist-get 'cc:attributionURL alist))
	 (orgauthor (if attributionname
			(if attributionurl
			    (format "by [[%s][%s]]" attributionurl attributionname)
			  (format "by %s" attributionname))
		      ""))
	 (htmlauthor (if attributionname
			 (if attributionurl
			     (format "by <a rel=\"dc:creator\" href=\"%s\" property=\"cc:attributionName\">%s</a>"
				     attributionurl attributionname)
			   (format "by <span property=\"dc:creator cc:attributionName\">%s</span>" attributionname))
		       ""))
	 (title (alist-get 'dc:title alist "Image"))
	 (htmltitle (format "<span property=\"dc:title\">%s</span>" title))
	 (imgalt (or (alist-get 'imgalt alist)
		     title))
	 (imgadapted (alist-get 'imgadapted alist "from"))
	 (sourceuri (alist-get 'dc:source alist))
	 (sourcetext (alist-get 'sourcetext alist))
	 (sourcehtml (format "; <a rel=\"dc:source\" href=\"%s\">%s %s</a>"
			     sourceuri imgadapted sourcetext))
	 (texwidth (alist-get 'texwidth alist 0.9))
	 (orglicense (format "%s %s under [[%s][%s]]; %s [[%s][%s]]"
			     title orgauthor licenseurl licensetext
			     imgadapted sourceuri sourcetext))
	 (htmllicense (format "<p>%s %s under <a rel=\"license\" href=\"%s\">%s</a>%s</p>"
			      htmltitle htmlauthor licenseurl licensetext
			      sourcehtml))
	 (texlicense (replace-regexp-in-string
		      "\n" "" (org-export-string-as orglicense 'latex t)))
	 )
    (if caption
	(concat (format "@@html: </p><div class=\"imgcontainer\"><div about=\"%s\" class=\"figure\"><p><img src=\"%s\" alt=\"%s\" /></p><p>%s</p>%s</div></div><p>@@"
			filename filename imgalt caption htmllicense)
		"\n"
		(format "#+BEGIN_EXPORT latex\n\\begin{figure}[htp]\n  \\centering\n  \\includegraphics[width=%s\\linewidth]{%s}\n  \\caption{%s (%s)}\n\\end{figure}\n#+END_EXPORT\n"
			texwidth filename caption texlicense))
      (concat (format "@@html: <div about=\"%s\" class=\"figure\"><p><img src=\"%s\" alt=\"%s\" /></p><p></p>%s</div>@@"
		      filename filename imgalt htmllicense)
	      "\n"
	      (format "#+BEGIN_EXPORT latex\n\\begin{figure}[htp]\n  \\centering\n  \\includegraphics[width=%s\\linewidth]{%s}\n  \\caption{%s}\n\\end{figure}\n#+END_EXPORT\n"
			texwidth filename texlicense)))))

(provide 'reveal-config)
;;; reveal-config.el ends here
