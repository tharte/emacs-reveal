<!--- Local IspellDict: en -->

I’m experimenting with lecture slides using
[reveal.js](https://github.com/hakimel/reveal.js/)
(see [here for a live demo of reveal.js](http://lab.hakim.se/reveal-js/)),
which is an HTML presentation framework.
Thus, there is no need to use special-purpose presentation software
such as LibreOffice Impress or its proprietary counterparts such as
Powerpoint.  Presentations can be viewed on *any device* using a Web
browser, with its multimedia support for embedded audio explanations
(or videos).

I create presentations in my usual computing environment, namely
[GNU Emacs](https://www.gnu.org/software/emacs/),
in a simple text format called [Org Mode](http://orgmode.org/), which
can be exported to reveal.js thanks to
[Org-Reveal](https://github.com/yjwen/org-reveal/).
That way, I can focus on slides’ contents in a highly intuitive plain
text document, which can be exported to HTML for presentation in a Web
browser.

My requirements, which lead me to select reveal.js, are as follows:
 1. Free software (really, I mean
    [free](https://fsfe.org/about/basics/freesoftware.en.html), not
    gratis) that is usable on lots of devices
 2. Embed audio recordings for self-contained presentations
 3. Focus on contents, not on layout while creating presentations
 4. Allow collaboration, ideally with support for versions and diffs

First, I tried
[LaTeX with the Beamer package](https://en.wikibooks.org/wiki/LaTeX/Presentations),
where presentations are created as PDF documents and
which seems to meet all requirements.  However, although audio can be
embedded, I found audio support in free PDF readers unsatisfactory.

Next, I tried [LibreOffice Impress](https://www.libreoffice.org/).
I perceived the creation of presentations as too cumbersome, in
particular embedding of audio files.  Also, Impress does neither meet
requirements (3) nor (4).

Eventually, I ended up with reveal.js.
To embed audio, I’m using the
[reveal.js plugin audio-slideshow](https://github.com/rajgoel/reveal.js-plugins)
with minor additions, which you can find in
[my fork of audio-slideshow](https://github.com/lechten/reveal.js-plugins).
To make that plugin work with Org-Reveal, I extended that in
[my fork of Org-Reveal](https://github.com/lechten/org-reveal).

My setup is as follows:
 * GNU Emacs
   * Org mode (from ELPA; the version included in Emacs is too old)
   * Org-reveal (from my
     [repository](https://github.com/lechten/org-reveal))
   * Emacs libraries org-ref (for citations) and htmlize (for source
     code highlighting)
 * reveal.js
   * reveal.js-plugins (from my
     [repository](https://github.com/lechten/reveal.js-plugins))
   * [reveal.js-jump-plugin](https://github.com/SethosII/reveal.js-jump-plugin)
   * [Reveal.js-TOC-Progress](https://github.com/e-gor/Reveal.js-TOC-Progress)

My Emacs initialization code for the above is included in
[this file](reveal-config.el), which you can load from your `~/.emacs`
(or embed there).

My [course on Operating Systems](https://gitlab.com/oer/OS) is a
real-world use case for the above.  Presentations are built automatically
upon commit by a GitLab runner (see its
[configuration file](https://gitlab.com/oer/OS/blob/master/.gitlab-ci.yml)
for details).

To build HTML presentations manually from org source files, do this:

	$ git clone https://gitlab.com/oer/OS.git
	$ cd OS
	$ git submodule sync --recursive
	$ git submodule update --init --recursive
	$ emacs --batch --load elisp/install.el --funcall install
	$ emacs --batch --load elisp/publish.el --funcall org-publish-all

As usual, use `git pull` to update the source directory later on.
Included submodules need to be updated separately, though, with
`git submodule update --init --recursive`.  The first `emacs`
invocation above installs necessary packages, which is only necessary
once.  The second one publishes the HTML presentation into the
subdirectory `public`.
