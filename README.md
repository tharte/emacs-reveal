<!--- Local IspellDict: en -->

This project provides free software to create presentations (slides with
audio) that are suitable as
[Open Educational Resources (OER)](https://en.wikipedia.org/wiki/Open_educational_resources),
which in my view comes with two frequently neglected requirements:
1. Layout and style of presentations are separated from content to
   simplify *collaboration across organizational boundaries*.
2. Slides’ content is written down in simple text files, which enables
   *comparisons* of adapted or enhanced *versions* (with `diff`-like
   functionality, e.g, with the
   [compare functionality on GitLab](https://gitlab.com/oer/OS/compare/os02...os03)).

During summer term 2017 I created the software in this repository to
produce lecture slides for a course on Operating Systems using the
HTML presentation framework [reveal.js](https://github.com/hakimel/reveal.js/)
(see [here for the official reveal.js demo](http://lab.hakim.se/reveal-js/)
and [here for my presentations on Operating Systems](https://oer.gitlab.io/OS/)).
As reveal.js presentations are in HTML format, there is no need for
students to use special-purpose presentation software such as
LibreOffice Impress or its proprietary counterparts such as
Powerpoint.  Instead, presentations can be viewed on *any device*
using a Web browser, with its multimedia support for embedded audio
explanations (or videos).

I intend to use the software of this repository for other courses (and
talks) as well.  Feel free to use and adapt for your purposes!

Although reveal.js is an HTML presentation framework, I do not create
presentations in HTML.  Instead, I rely on my usual computing
environment, namely the text editor
[GNU Emacs](https://www.gnu.org/software/emacs/), in a simple
text format called [Org Mode](http://orgmode.org/), which can be
exported to reveal.js thanks to
[Org-Reveal](https://github.com/yjwen/org-reveal/).
That way, I can focus on slides’ contents in a highly intuitive plain
text document, which can be exported to HTML for presentation in a Web
browser (besides, presentations can be downloaded in two different
layouts in PDF format in response to students’ requests:
one PDF version contains one page per slide while the other one is
generated via [LaTeX](https://www.latex-project.org/)).  All layout
information is kept separately from content, so little effort is
necessary to adapt presentations to different styles or corporate
identities.

My requirements, which lead me to select reveal.js, are as follows:
 1. Free software (really, I mean
    [free](https://fsfe.org/about/basics/freesoftware.en.html), not
    gratis) that is usable on lots of devices
 2. Embed audio recordings for self-contained presentations
 3. Focus on content, not on layout while creating presentations
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
[reveal.js plugin audio-slideshow](https://github.com/rajgoel/reveal.js-plugins).
To make that plugin work with Org-Reveal, I extended
[Org-Reveal in my fork](https://github.com/lechten/org-reveal).

The setup is as follows:
 * GNU Emacs
   * Org mode (from ELPA; the version included in Emacs is too old)
   * Org-reveal (from my
     [repository](https://github.com/lechten/org-reveal))
   * Emacs libraries org-ref (for citations) and htmlize (for source
     code highlighting)
 * reveal.js
   * [reveal.js-plugins](https://github.com/rajgoel/reveal.js-plugins.git)
   * [reveal.js-jump-plugin](https://github.com/SethosII/reveal.js-jump-plugin)
   * [Reveal.js-TOC-Progress](https://github.com/e-gor/Reveal.js-TOC-Progress)
 * LaTeX for PDF generation

Emacs initialization code for the above is included in
[this file](reveal-config.el), which you can load from your `~/.emacs`
(or embed there).

My [course on Operating Systems](https://gitlab.com/oer/OS) is a
real-world use case for the above.  Presentations are built automatically
using Continuous Integration (CI) upon commit by a GitLab runner (see its
[configuration file](https://gitlab.com/oer/OS/blob/master/.gitlab-ci.yml)
for details), which publishes the
[presentations as GitLab pages](https://oer.gitlab.io/OS/).
The [Docker image used by the GitLab runner](https://gitlab.com/oer/docker)
contains necessary underlying software such as GNU Emacs and LaTeX.

This [[https://gitlab.com/oer/emacs-reveal-howto][Howto]] explains how
to use emacs-reveal based on a small sample presentation.  For a
real-world course on Operating Systems, you could also do the following
to build all HTML presentations manually from `org` source files (into
sub-directory `public`):

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
subdirectory `public`.  (From within Emacs, you can generate the HTML
presentation for an individual `org` file using the usual export
functionality by pressing `C-c C-e R B`.)
