<!--- Local IspellDict: en -->
<!--- SPDX-License-Identifier: GPL-3.0-or-later -->

This repository provides *emacs-reveal*, which is
[free/libre and open source software (FLOSS)](https://en.wikipedia.org/wiki/Free_and_open-source_software)
to create presentations (slides with audio) that are suitable as
[Open Educational Resources (OER)](https://en.wikipedia.org/wiki/Open_educational_resources),
which in my view comes with two frequently neglected requirements:
1. Layout and style of presentations are separated from content to
   simplify *collaboration across organizational boundaries*.
2. Slides’ content is written down in simple text files, which enables
   *comparisons* of adapted or enhanced *versions* (with `diff`-like
   functionality, e.g, with the
   [compare functionality on GitLab](https://gitlab.com/oer/OS/compare/os02...os03)).

During summer term 2017 I created emacs-reveal to
produce lecture slides (initially for a course on Operating Systems)
and presentations as OER using the
HTML presentation framework [reveal.js](https://github.com/hakimel/reveal.js/)
(see [here for the official reveal.js demo](https://revealjs.com/)
and [here for my presentations on Operating Systems](https://oer.gitlab.io/OS/)).

On the students’ side, as reveal.js presentations are in HTML format,
there is no need for students to use special-purpose presentation
software such as LibreOffice Impress or its proprietary counterparts
such as Powerpoint.  Instead, presentations can be viewed on *any
device* using a Web browser, with its multimedia support for embedded
audio explanations (or videos).

Since its creation, I’ve been updating my infrastructure continuously.
In particular, sharing of OER figures with proper attribution and
license information has been simplified considerably; see the project
hosting [OER figures](https://gitlab.com/oer/figures/) for more
details on my approach towards meta-data, which is based on standard
vocabularies and embeds license information using
[RDFa](https://wiki.creativecommons.org/wiki/RDFa) in HTML, making it
accessible on the
[Semantic Web](https://en.wikipedia.org/wiki/Semantic_Web).

I’m using emacs-reveal for my courses and talks in general.  Feel free
to use and adapt for your purposes!

Although reveal.js is an HTML presentation framework, I do not create
presentations in HTML.  Instead, I rely on my usual computing
environment, namely the text editor
[GNU Emacs](https://www.gnu.org/software/emacs/), in a simple
text format called [Org mode](https://orgmode.org/), which can be
exported to reveal.js thanks to
[Org-Reveal](https://github.com/yjwen/org-reveal/), for which I
maintain [the forked variant org-re-reveal](https://gitlab.com/oer/org-re-reveal)
with an extension for citations and bibliography, namely
[org-re-reveal-ref](https://gitlab.com/oer/org-re-reveal-ref).
That way, I can focus on slides’ contents in a highly intuitive plain
text document, which can be exported to HTML for presentation in a Web
browser (besides, presentations can be downloaded in two different
layouts in PDF format in response to students’ requests:
one PDF version contains one page per slide while the other one is
generated via [LaTeX](https://www.latex-project.org/) from Org source
documents).  All layout
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
[audio-slideshow plugin of reveal.js](https://github.com/rajgoel/reveal.js-plugins).

The setup is as follows:
 * GNU Emacs
   * Org mode (recent versions, e.g., from ELPA)
   * Org-re-reveal-ref (from MELPA or
     [my repository](https://gitlab.com/oer/org-re-reveal-ref))
   * Emacs library htmlize (for source code highlighting)
 * reveal.js
   * [reveal.js-plugins](https://github.com/rajgoel/reveal.js-plugins)
   * [reveal.js-coursemod](https://github.com/Sonaryr/reveal.js-coursemod)
   * [reveal.js-jump-plugin](https://github.com/SethosII/reveal.js-jump-plugin)
   * [reveal.js-quiz](https://gitlab.com/schaepermeier/reveal.js-quiz)
   * [Reveal.js-TOC-Progress](https://github.com/e-gor/Reveal.js-TOC-Progress)
 * LaTeX for PDF generation

Emacs initialization code for the above is included in
[the file `emacs-reveal.el`](emacs-reveal.el), which you can load
from your `~/.emacs` (or embed there).

A [Howto](https://gitlab.com/oer/emacs-reveal-howto) provides a small
sample presentation generated with emacs-reveal that explains how
to use emacs-reveal.  Besides,
the [README of org-re-reveal](https://gitlab.com/oer/org-re-reveal)
documents various features and options of org-re-reveal, which are also
available with emacs-reveal.

My [course on Operating Systems with OER presentations](https://gitlab.com/oer/OS) is a
real-world use case for emacs-reveal.  Presentations are built automatically
using Continuous Integration (CI) upon commit by a GitLab runner (see its
[configuration file](https://gitlab.com/oer/OS/blob/master/.gitlab-ci.yml)
for details), which publishes the
[presentations as GitLab pages](https://oer.gitlab.io/OS/).
The [Docker image used by the GitLab runner](https://gitlab.com/oer/docker)
contains necessary underlying software such as GNU Emacs and LaTeX.

Of course, presentations can also be built locally (without Docker).
For my course on Operating Systems you could also do the following to
build all HTML presentations manually from `org` source files (into
sub-directory `public`):

	$ git clone https://gitlab.com/oer/OS.git
	$ cd OS
	$ git submodule sync --recursive
	$ git submodule update --init --recursive
	$ emacs --batch --load emacs-reveal/install.el --funcall install
	$ emacs --batch --load elisp/publish.el --funcall org-publish-all

As usual, use `git pull` to update the source directory later on.
Included submodules need to be updated separately, though, with
`git submodule update --recursive --remote`.  The first `emacs`
invocation above installs necessary packages, which is only necessary
once.  The second one publishes the HTML presentation into the
subdirectory `public`.  (From within Emacs, you can generate the HTML
presentation for an individual `org` file using Org’s export
functionality by pressing a key such as `C-c C-e v b`; the precise key
binding changed with different versions of the back-end.)
