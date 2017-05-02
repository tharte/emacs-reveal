;;; Org with reveal.js

;; Install org-reveal from GitHub for external plugin support.
;; Configure the path here.
(add-to-list 'load-path (expand-file-name "~/emacs/src/org-reveal"))
(require 'ox-reveal)

;; Where to find reveal.js?  Here as well as in other paths,
;; *relative* paths are preferable to prevent cross-origin issues with
;; firefox.
;; (setq org-reveal-root "./reveal.js") ; this is the default


;;; Configure a local MathJax installation.
;; Beware!  I needed to add the config parameter to the path, as
;; below.  Otherwise, MathJax did not work.
;; Note that the math plugin of reveal.js does not need to be
;; configured separately.
(setq org-reveal-mathjax-url
      "./MathJax-2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML")


;;; Configure reveal.js plugins.
;; Note that in the relative src-paths in org-reveal-external-plugins,
;; %s is automatically replaced with org-reveal-root.

;;(setq org-reveal-external-plugins nil)
;; Activate audio-slideshow plugin
(add-to-list 'org-reveal-external-plugins
      (cons 'audio-slideshow
	    " { src: '%splugin/audio-slideshow/audio-slideshow.js', condition: function( ) { return !!document.body.classList; } }"))
;; I do not want to see the audio player if no local audio file is given.
(setq org-reveal-init-script "  audio: { autoplay: true, onlyIfLocalAudio: true, externalPlayerCSS: true }")

;; Activate TOC progress plugin
;; If there are lots of subsections, 'scroll'ing can be enabled or
;; the font size can be 'reduce'd.  Go for the latter.
(add-to-list 'org-reveal-external-plugins
	     (cons 'toc-progress
		   " { src: '%splugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize('reduce', 'rgba(120,138,130,0.2)'); toc_progress.create(); } }"))
;; For multiple CSS files, separate them with "\n" in org-reveal-extra-css:
(setq org-reveal-extra-css
      (concat org-reveal-root "/plugin/toc-progress/toc-progress.css"))


;;; Use org-ref to enable citations.
;; With org-reveal, only slide titles can be link targets.
;; Setup Bibliography in HTML accordingly.
(require 'org-ref)
(setq org-ref-bib-html "</p></section>
<section id=\"slide-bibliography\">
<h3 class=\"org-ref-bib-h3\">Bibliography</h3>
<p>\n")
(setq org-ref-ref-html
      "<a class=\"org-ref-reference\" href=\"#slide-bibliography\">[%s]</a>")

;; Display books differently.  Remaining entries are defaults.
(setq org-ref-bibliography-entry-format
      '(("article" . "%a, %t, <i>%j</i>, <b>%v(%n)</b>, %p (%y). <a href=\"%U\">link</a>. <a href=\"http://dx.doi.org/%D\">doi</a>.")
	("book" . "%a, %t, %u, %y.</a><br /><a href=\"%U\">%U")
	("techreport" . "%a, %t, %i, %u (%y).")
	("proceedings" . "%e, %t in %S, %u (%y).")
	("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)")))
