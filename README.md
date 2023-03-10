<!--- Local IspellDict: en -->
<!--- SPDX-FileCopyrightText: 2017-2022 Jens Lechtenbörger -->
<!--- SPDX-License-Identifier: GPL-3.0-or-later -->

[![DOI](https://jose.theoj.org/papers/10.21105/jose.00050/status.svg)](https://doi.org/10.21105/jose.00050)
[![REUSE status](https://api.reuse.software/badge/gitlab.com/oer/emacs-reveal)](https://api.reuse.software/info/gitlab.com/oer/emacs-reveal)

Pseudo-forked from the [emacs-reveal](https://gitlab.com/oer/emacs-reveal) repo on Gitlab 
(can't fork from GitLab to GitHub, unfortunately). Add:

```
git remote add upstream  https://gitlab.com/oer/emacs-reveal
```

This repository provides *emacs-reveal*, which is
[free/libre and open source software (FLOSS)](https://en.wikipedia.org/wiki/Free_and_open-source_software)
to create [Open Educational Resources (OER)](https://en.wikipedia.org/wiki/Open_educational_resources)
in the form of [reveal.js](https://revealjs.com/) presentations (HTML
slides with audio).  As the name suggests, emacs-reveal is based on
the text editor [GNU Emacs](https://www.gnu.org/software/emacs/).
More specifically, authors write their OER in a lightweight markup
language called [Org mode](https://orgmode.org/), which is native to
GNU Emacs but can be edited everywhere (as text file; also with
“real” support in [other editors](https://orgmode.org/install.html#editors))
and which allows authors (a) to
collaborate on source documents via version control systems such as
Git and (b) to focus on contents, while layout information is
maintained separately.  Org files can then be exported to various
target formats (e.g., reveal.js presentations or PDF documents).

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
- [Evaluation results](#evaluation-results)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Relationships between Emacs packages

| Package                                                                 | Description                                                                                                                                      |
|-------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| [org-reveal](https://github.com/yjwen/org-reveal)                       | Origin of org-re-reveal                                                                                                                          |
| [org-re-reveal](https://gitlab.com/oer/org-re-reveal)                   | Fork of org-reveal, initially to add audio fragments, now with [various changes](https://gitlab.com/oer/org-re-reveal/blob/master/CHANGELOG.org) |
| [org-re-reveal-ref](https://gitlab.com/oer/org-re-reveal-ref)           | Addon to org-re-reveal for bibliography slide based on [org-ref](https://github.com/jkitchin/org-ref)                                            |
| [org-re-reveal-citeproc](https://gitlab.com/oer/org-re-reveal-citeproc) | Alternative to org-re-reveal-ref for bibliography slide based on syntax of Org mode 9.5                                                          |
| [oer-reveal](https://gitlab.com/oer/oer-reveal)                         | Export backend derived from org-re-reveal; functionality for installation of reveal.js and plugins; simplification of licensing for OER          |
| [emacs-reveal](https://gitlab.com/oer/emacs-reveal/)                    | Bundling of org-re-reveal, org-re-reveal-citeproc, org-re-reveal-ref, and oer-reveal                                                             |

Originally, *emacs-reveal* was created to enhance *org-reveal*, and it
contained the code of what is now maintained separately as
*org-re-reveal-ref* and *oer-reveal*.  Those packages were separated
to make as much of *emacs-reveal* available on the Emacs package
repository [MELPA](https://melpa.org/) as possible.
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
([published version](https://dx.doi.org/10.18420/delfi2019_280))
for more details on my approach towards meta-data, which is based on
standard vocabularies and embeds machine-readable license information using
the [Creative Commons Rights Expression Language (CC REL)](https://labs.creativecommons.org/2011/ccrel-guide/)
(with [RDFa](https://wiki.creativecommons.org/wiki/RDFa) in HTML),
(a) avoiding manual identification and copying of licensing
information and (b) making licensing information accessible on the
[Semantic Web](https://en.wikipedia.org/wiki/Semantic_Web).

Beyond that paper’s contents, emacs-reveal supports the specification
of a presentation’s primary license with SPDX headers (see the
[REUSE project](https://reuse.software/) for more information on such
headers), from which license information can be generated (“Except
where otherwise noted, the work x by y is published under the license
z”), e.g.,
[that slide](https://oer.gitlab.io/emacs-reveal-howto/howto.html#slide-license),
which includes machine-readable RDFa markup (the
[PDF version of that presentation](https://oer.gitlab.io/emacs-reveal-howto/howto.pdf)
just contains a human readable variant).

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
its extensions [org-re-reveal-ref](https://gitlab.com/oer/org-re-reveal-ref)
and [org-re-reveal-citeproc](https://gitlab.com/oer/org-re-reveal-citeproc)
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
   * Org-re-reveal, org-re-reveal-citeproc, org-re-reveal-ref, oer-reveal
     (from MELPA or [my repositories](https://gitlab.com/oer))
   * Htmlize (for source code highlighting)
 * reveal.js
   * [reveal-a11y](https://github.com/marcysutton/reveal-a11y)
     (for improved accessibility)
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
2. Use Docker image `registry.gitlab.com/oer/emacs-reveal/emacs-reveal` from
   [this container registry](https://gitlab.com/oer/emacs-reveal/container_registry)
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
docker run --rm --name emacs-reveal -it registry.gitlab.com/oer/emacs-reveal/emacs-reveal
```

For GUI support, you need to set up authentication information for the
Docker container.  The details depend on your environment.  If you
have got an environment variable `XAUTHORITY` in your host system, the
following should work:

```
docker run --rm --name emacs-reveal -it --net=host -e DISPLAY -v $XAUTHORITY:/root/.Xauthority registry.gitlab.com/oer/emacs-reveal/emacs-reveal
```

Alternatively, if you have got a file `~/.Xauthority`:

```
docker run --rm --name emacs-reveal -it --net=host -e DISPLAY -v $HOME/.Xauthority:/root/.Xauthority registry.gitlab.com/oer/emacs-reveal/emacs-reveal
```

In any case, a shell opens in the container.  Type `emacs` to start
GNU Emacs, load an `org` file (e.g., `Readme.org` coming with
`org-re-reveal`, available under directory
`/root/.emacs.d/elpa/emacs-reveal/org-re-reveal`)
and export the reveal.js presentation (with function
`oer-reveal-export-to-html`, bound to key `C-c C-e w w`).

Copy the generated HTML presentation (and resources `local.css` as well as
`images/`) from the container to a directory with your `reveal.js`
installation.  E.g., use commands such as the following:

```
docker cp emacs-reveal:/root/.emacs.d/elpa/emacs-reveal/org-re-reveal/Readme.html .
```

Open presentation in browser.

In general, copying files to and from the container can be avoided by
binding your directory with `org` files into the container (with
option `-v`):

```
docker run --rm --name emacs-reveal -it -v /some/dir/with/org-files:/tmp registry.gitlab.com/oer/emacs-reveal/emacs-reveal
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
[this GitLab repository](https://gitlab.com/oer/emacs-reveal).

Before cloning the repository, you may want to think about two options
of using emacs-reveal.  First, you can stick with the default “managed
mode” as defined by the customizable variable
`emacs-reveal-managed-install-p`.  With its default value of `t`,
emacs-reveal initializes and updates its submodules (Lisp packages
for Org mode, org-re-reveal, org-re-reveal-citeproc or org-re-reveal-ref,
oer-reveal and reveal.js with several submodules).
In that case, you do not need to
install any of the bundled software yourself, and you may want to
clone the Git repository of emacs-reveal recursively, downloading
161 MB as of March 2020 (if you clone without option `--recursive`,
you just download 1.1 MB, but emacs-reveal will download the remaining
components upon its first use, slowing down first use):

```
git clone --recursive https://gitlab.com/oer/emacs-reveal.git
```

Then, activate emacs-reveal by loading it from your `~/.emacs`:

```
(add-to-list 'load-path "<path-where-you-cloned>/emacs-reveal")
(require 'emacs-reveal)
```

If you decide to manage the bundled components yourself, clone without
option `--recursive` and use the following in `~/.emacs`:

```
(add-to-list 'load-path "<path-where-you-cloned>/emacs-reveal")
(setq emacs-reveal-managed-install-p nil)
(require 'emacs-reveal)
```

After restarting Emacs, emacs-reveal checks whether its dependencies
are installed; if not, you are asked whether emacs-reveal
should install it from [MELPA](https://melpa.org/) for you.

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

# Evaluation results

I'm aware that my presentation format deviates from university
standards, which creates initial challenges.  I believe to have good
reasons for that format (see
[that presentation for my interpretation of Just-in-Time Teaching (JiTT)](https://oer.gitlab.io/OS/Operating-Systems-JiTT.html)
or [that paper in German](https://doi.org/10.21240/mpaed/34/2019.03.02.X)),
and the following figure contains some evaluation results for a
[course on Operating Systems](https://gitlab.com/oer/OS)
indicating that students do not disagree in general, although the
picture changed in 2021 (see figure with evaluation
results below): students started to state a strong preference for
videos, and the perceived usefulness of hyperlinks decreased.
(About a third in 2022 voted in favor of a “video mode”.)
I plan to repeat such evaluations…

These are some changes based on student feedback:
I added different types of hyperlinks in 2018 and extended them in
2019 and 2021; thus there is no data for 2017.  I disabled auto-play
for audio in 2019; only a minority (nobody in 2021) asked to revert
this change, which can be performed with URL parameter
`audio-autoplay`).  Since August 2021, hyperlinks to external contents
open in new tabs, and I switched presentations to use the `linear`
navigation mode of reveal.js (unless URL parameter
`default-navigation` is used).  In June 2022, I added URL parameter
`audio-speed`.

![Evaluation results for emacs-reveal](https://gitlab.com/oer/figures/-/raw/master/org/emacs-reveal/evaluation.png "Evaluation results for emacs-reveal")
