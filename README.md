<!--- Local IspellDict: en -->
<!--- SPDX-FileCopyrightText: 2017-2020 Jens Lechtenbörger -->
<!--- SPDX-License-Identifier: GPL-3.0-or-later -->

[![DOI](https://jose.theoj.org/papers/10.21105/jose.00050/status.svg)](https://doi.org/10.21105/jose.00050)
[![REUSE status](https://api.reuse.software/badge/gitlab.com/oer/emacs-reveal)](https://api.reuse.software/info/gitlab.com/oer/emacs-reveal)

This repository provides *emacs-reveal*, which is
[free/libre and open source software (FLOSS)](https://en.wikipedia.org/wiki/Free_and_open-source_software)
to create HTML presentations ([reveal.js](https://revealjs.com/)
slides with audio) and PDF documents for those presentations
from [Org mode](https://orgmode.org/) source files.
Emacs-reveal satisfies several requirements in a novel way to
ensure that resulting presentations and source files are
suitable as [Open Educational Resources (OER)](https://en.wikipedia.org/wiki/Open_educational_resources).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Relationships between Emacs packages](#relationships-between-emacs-packages)
- [Introduction](#introduction)
- [Technical Background](#technical-background)
- [Publications on emacs-reveal](#publications-on-emacs-reveal)
- [Usage of emacs-reveal](#usage-of-emacs-reveal)
  - [Emacs-reveal in GitLab CI/CD](#emacs-reveal-in-gitlab-cicd)
  - [Emacs-reveal with Docker](#emacs-reveal-with-docker)
  - [Emacs-reveal for daily use](#emacs-reveal-for-daily-use)
- [Emacs-reveal for a course on Operating Systems](#emacs-reveal-for-a-course-on-operating-systems)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Relationships between Emacs packages

| Package                                                       | Description                                                                                                                                      |
|---------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| [org-reveal](https://github.com/yjwen/org-reveal)             | Origin of org-re-reveal                                                                                                                          |
| [org-re-reveal](https://gitlab.com/oer/org-re-reveal)         | Fork of org-reveal, initially to add audio fragments, now with [various changes](https://gitlab.com/oer/org-re-reveal/blob/master/CHANGELOG.org) |
| [org-re-reveal-ref](https://gitlab.com/oer/org-re-reveal-ref) | Addon to org-re-reveal for bibliography slide based on [org-ref](https://github.com/jkitchin/org-ref)                                            |
| [oer-reveal](https://gitlab.com/oer/oer-reveal)               | Export backend derived from org-re-reveal; functionality for installation of reveal.js and plugins; simplification of licensing for OER          |
| [emacs-reveal](https://gitlab.com/oer/emacs-reveal/)          | Bundling of org-re-reveal, org-re-reveal-ref, and oer-reveal                                                                                     |

Originally, *emacs-reveal* was created to enhance *org-reveal*, and it
contained the code of what is now maintained separately as
*org-re-reveal-ref* and *oer-reveal*.  Those packages were separated
to make as much of *emacs-reveal* available on MELPA as possible.
Whether anyone wants to use those package in isolation is up to them.

# Introduction

During summer term 2017 I created emacs-reveal to
produce lecture slides (initially for a course on Operating Systems)
and presentations as OER under the
[Creative Commons license](https://creativecommons.org/use-remix/cc-licenses/)
[CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)
using the
HTML presentation framework [reveal.js](https://github.com/hakimel/reveal.js/)
(see [here for my presentations on Operating Systems](https://oer.gitlab.io/OS/)).

Since its creation, I’ve been updating emacs-reveal and the
surrounding infrastructure continuously.  In particular, sharing of
OER figures with proper attribution and license information has been
simplified considerably; see the project hosting
[OER figures](https://gitlab.com/oer/figures/) and
[this paper](https://gitlab.com/lechten/publications/blob/master/delfi2019/delfi.org)
for more details on my approach towards meta-data, which is based on
standard vocabularies and embeds license information using
the [Creative Commons Rights Expression Language (CC REL)](https://labs.creativecommons.org/2011/ccrel-guide/)
(with [RDFa](https://wiki.creativecommons.org/wiki/RDFa) in HTML),
(a) avoiding manual identification and copying of licensing
information and (b) making licensing information accessible on the
[Semantic Web](https://en.wikipedia.org/wiki/Semantic_Web).

I’m using emacs-reveal for my courses and talks in general.  Feel free
to use and adapt for your purposes!

# Technical Background

Emacs-reveal addresses several technical requirements for OER
(explained in detail in
[this paper (in German)](https://www.medienpaed.com/article/view/651)):
1. Layout and style of presentations are separated from content to
   simplify *collaboration across organizational boundaries*.
2. Slides’ content is written down in simple text files, which enables
   *comparisons* of adapted or enhanced *versions* (with `diff`-like
   functionality, e.g, with the
   [compare functionality on GitLab](https://gitlab.com/oer/OS/compare/os02...os03)).
3. Presentations are usable in standard Web browsers with their
   multimedia support on almost *any device*, without the need for
   special-purpose software.  Offline use is supported.

Although reveal.js is an HTML presentation framework, I do not create
presentations in HTML.  Instead, I rely on my usual computing
environment, namely the text editor
[GNU Emacs](https://www.gnu.org/software/emacs/), in a simple
text format called [Org mode](https://orgmode.org/), which can be
exported to reveal.js thanks to
[Org-Reveal](https://github.com/yjwen/org-reveal/), for which I
maintain [the forked variant org-re-reveal](https://gitlab.com/oer/org-re-reveal),
its extension [org-re-reveal-ref](https://gitlab.com/oer/org-re-reveal-ref)
for citations and bibliography, and
[oer-reveal](https://gitlab.com/oer/oer-reveal) with further OER resources.
That way, I can focus on slides’ contents in a highly intuitive plain
text document, which can be exported to HTML for presentation in a Web
browser (besides, presentations can be downloaded in two different
layouts in PDF format in response to students’ requests:
one PDF version contains one page per slide while the other one is
a more concise version generated via [LaTeX](https://www.latex-project.org/)
from Org source documents).  All layout
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
 * GNU Emacs with the following packages
   * Org mode (recent versions, e.g., from ELPA)
   * Org-re-reveal, org-re-reveal-ref, oer-reveal (from MELPA or
     [my repositories](https://gitlab.com/oer))
   * Htmlize (for source code highlighting)
 * reveal.js
   * [reveal.js-plugins](https://github.com/rajgoel/reveal.js-plugins)
     (for audio explanations and SVG animations)
   * [reveal.js-coursemod](https://github.com/Sonaryr/reveal.js-coursemod)
     (for a courseware view where notes are visible)
   * [reveal.js-jump-plugin](https://github.com/SethosII/reveal.js-jump-plugin)
     (for jumps to slides by entering their numbers)
   * [reveal.js-quiz](https://gitlab.com/schaepermeier/reveal.js-quiz)
     (for embedded quizzes supporting retrieval practice)
   * [Reveal.js-TOC-Progress](https://github.com/e-gor/Reveal.js-TOC-Progress)
     (for a hyperlinked table of contents as slide footer)
 * LaTeX for PDF generation

# Publications on emacs-reveal

* [Emacs-reveal: A software bundle to create OER presentations](https://doi.org/10.21105/jose.00050)
* [Erstellung und Weiterentwicklung von Open Educational Resources im Selbstversuch](https://www.medienpaed.com/article/view/651)
* [Simplifying license attribution for OER with emacs-reveal](https://dx.doi.org/10.18420/delfi2019_280)

# Usage of emacs-reveal

To use emacs-reveal, several options exist as sketched subsequently.

1. Use emacs-reveal inside [GitLab’s Continuous Integration](https://docs.gitlab.com/ce/ci/)
   infrastructure with automatic deployment of resulting presentations
   as [GitLab Pages](https://docs.gitlab.com/ce/user/project/pages/index.html).
2. Use docker image `registry.gitlab.com/oer/docker/emacs-reveal`.
3. Install GNU Emacs, emacs-reveal, and above packages.

## Emacs-reveal in GitLab CI/CD

A simple way to get started with the first option is to fork the
[emacs-reveal howto](https://gitlab.com/oer/emacs-reveal-howto) at
GitLab.  In your clone, change the file `howto.org` and commit the
change.  A GitLab runner will start automatically and publish the
presentation under your
[GitLab Pages namespace](https://docs.gitlab.com/ce/user/project/pages/getting_started_part_one.html).

## Emacs-reveal with Docker

For the second option, [install Docker](https://docs.docker.com/install/)
and start a container for emacs-reveal, e.g.:

```
docker run --rm --name emacs-reveal -it registry.gitlab.com/oer/docker/emacs-reveal
```

Or for GUI support:

```
docker run --rm --name emacs-reveal -it --net=host -e DISPLAY -v $HOME/.Xauthority:/root/.Xauthority registry.gitlab.com/oer/docker/emacs-reveal
```

In both cases, a shell opens in the container.  Type `emacs` to start
GNU Emacs, load an `org` file (e.g., `Readme.org` coming with
`org-re-reveal`, available under directory
`/root/.emacs.d/elpa/org-re-reveal-<some-version-specific-string>`)
and export the reveal.js presentation (either with function
`org-re-reveal-export-to-html`, bound to key `C-c C-e v v`, or
`oer-reveal-export-to-html`, bound to key `C-c C-e w w` for additional
reveal.js plugins).

Copy the generated HTML presentation (and resources `local.css` as well as
`images/`) from the container to a directory with your `reveal.js`
installation.  E.g., use commands such as the following:

```
docker cp emacs-reveal:/root/.emacs.d/elpa/org-re-reveal-<some-version-specific-string>/Readme.html .
```

Open presentation in browser.

In general, copying files to and from the container can be avoided by
binding your directory with `org` files into the container (with
option `-v`):

```
docker run --rm --name emacs-reveal -it -v /some/dir/with/org-files:/tmp registry.gitlab.com/oer/docker/emacs-reveal
```

Note that more powerful features of emacs-reveal based on plugins of
reveal.js listed above are not enabled by default when starting Emacs
inside the Docker container.  They require additional setup (which is
activated when publishing according to the first option with GitLab
CI/CD and also with the following option).

## Emacs-reveal for daily use

For the third option (my daily work environment), you need
[GNU Emacs](https://www.gnu.org/software/emacs/download.html),
a directory containing the contents of
[this GitLab repository](https://gitlab.com/oer/emacs-reveal)
(cloned via Git or extracted from an archive downloaded from GitLab, e.g.,
[a ZIP archive](https://gitlab.com/oer/oer-courses/vm-neuland/-/archive/master/vm-neuland-master.zip)),
and the packages mentioned above.  Once Emacs is installed,
the packages can be installed manually or with the following command:

```
emacs --batch --load emacs-reveal/install.el --funcall install
```

Emacs initialization code for all packages and reveal.js with its
plugins can be activated with [the file `emacs-reveal.el`](emacs-reveal.el),
which you can load from your `~/.emacs` (as done in
[.emacs](https://gitlab.com/oer/docker/blob/master/code/.emacs)
coming with the Docker image).

A [Howto](https://gitlab.com/oer/emacs-reveal-howto) provides a small
sample presentation generated with emacs-reveal that explains how to
use emacs-reveal.  Its publication code automatically loads
`emacs-reveal.el`.  Besides, the
[README of org-re-reveal](https://gitlab.com/oer/org-re-reveal)
mentioned above documents various features and options of
org-re-reveal, which are also available with emacs-reveal.

# Emacs-reveal for a course on Operating Systems

My [course on Operating Systems with OER presentations](https://gitlab.com/oer/OS)
is a real-world use case for emacs-reveal, combining all previously
mentioned aspects.  Presentations are built automatically using
Continuous Integration (CI) upon commit by a GitLab runner (see its
[configuration file](https://gitlab.com/oer/OS/blob/master/.gitlab-ci.yml)
for details), which publishes the
[presentations as GitLab pages](https://oer.gitlab.io/OS/).

The following builds presentations for my course on Operating Systems
locally (without Docker) from `org` source files into target directory
`public` (as of March 2019, the first `git` command alone downloads
about 150 MB of resources):

	$ git clone https://gitlab.com/oer/OS.git
	$ cd OS
	$ git submodule sync --recursive
	$ git submodule update --init --recursive
	$ emacs --batch --load emacs-reveal/install.el --funcall install
	$ emacs --batch --load elisp/publish.el

As usual, use `git pull` to update the source directory later on.
Included submodules need to be updated separately, though, with
`git submodule update --recursive --remote`.  The first `emacs`
invocation above installs necessary packages, which is only necessary
once.  The second one publishes the HTML presentation into the
subdirectory `public`.  (From within Emacs, you can generate the HTML
presentation for an individual `org` file using Org’s export
functionality by pressing keys for org-re-reveal or oer-reveal as
mentioned above.)
