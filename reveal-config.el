;;; reveal-config.el --- Configuration for org-reveal in GNU Emacs
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2017 Jens Lechtenbörger

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
;; With org-reveal, only slide titles can be link targets.
;; Setup Bibliography in HTML.
;; This is a dirty hack.  A paragraph is opened before the HTML below
;; is generated.  Thus, the paragraph must be closed first.  For valid
;; HTML, the org file must contain "@@html:<p>@@" after the bibliography
;; command.
(require 'org-ref)
(setq org-ref-bib-html "</p><div class=\"slide-footer\"><br></div></section>
<section id=\"slide-bibliography\" data-state=\"no-toc-progress\">
<h3 class=\"org-ref-bib-h3 no-toc-progress\">Bibliography</h3>\n")
(setq org-ref-ref-html
      "<a class=\"org-ref-reference\" href=\"#/slide-bibliography\">[%s]</a>")

;; Display article, book, and inproceedings differently.
;; Remaining entries are defaults.
(setq org-ref-bibliography-entry-format
      '(("article" . "%a, %t, <i>%j %v(%n)</i>, %p (%y). <a href=\"%U\">%U</a>")
	("book" . "%a, %t, %u, %y. <a href=\"%U\">%U</a>")
	("inproceedings" . "%a, %t, %b, %y. <a href=\"%U\">%U</a>")
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

(provide 'reveal-config)
;;; reveal-config.el ends here
