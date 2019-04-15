<!--- Local IspellDict: en -->
<!--- Copyright (C) 2019 Jens Lechtenbörger -->
<!--- SPDX-License-Identifier: CC-BY-4.0 -->
---
title: 'Emacs-reveal: A software bundle to create OER presentations with audio explanations'
tags:
  - slideshow
  - Open Educational Resources (OER)
  - GNU Emacs
  - reveal.js
authors:
  - name: Jens Lechtenbörger
    orcid: 0000-0002-3064-147X
    affiliation: 1
affiliations:
 - name: ERCIS, University of Münster, Germany
   index: 1
date: 15 April 2019
bibliography: paper.bib
---

# Summary

According to the *Ljubljana OER Action Plan 2017* [@Une17], “Open
Educational Resources (OER) support quality education that is
equitable, inclusive, open and participatory.”  However, several
challenges are known that hinder widespread creation, use, and re-use
of OER.  The first challenge identified in the Action Plan lies in
“the capacity of users to find, re-use, create,and share OER”, and the
first action category addressing that challenge, “Building awareness
and skills to use OER”, lists ten actions, among which action (c)
reads as follows: “Disseminate the findings of research on OER to
support models of good practice with a focus on cost-effectiveness,
sustainability, exploration of new tools and technologies for the
creation and sharing of OER”.

Emacs-reveal [@emacs-reveal] is a Free/Libre and Open Source Software
(FLOSS) bundle (combining novel and established FLOSS components) in
accordance with action (c).  Briefly, emacs-reveal is a software tool,
embedded in a powerful Continuous Integration infrastructure, to
create and (re-) use OER slideshows (for courses and talks) with with
references and embedded multimedia contents such as figures, audio
explanations, animations, videos, quizzes, and live code execution.
The software simplifies creation and re-use of OER by addressing
OER-specific requirements as summarized next.  For educational
resources to be free and open, next to proper licensing requirements
also technical requirements exist (as defined in the ALMS framework
[@HWSJ10], extended in [@Lec19]):

- OER should be usable (for learning) with FLOSS
  on (almost) any device, also mobile and offline.
- OER should be editable with FLOSS
  (this requires source file access).
- OER should be re-usable under the Single Sourcing paradigm (see
  [@Roc01]), which enables reuse and revision from a single,
  consistent source without copy&paste (copy&paste creates isolated
  copies, where the reconciliation of changes and improvements by
  different individuals would be almost impossible).
- OER should offer a separation of contents from layout (then, experts
  for content do not need to be design experts as well; also,
  cross-organizational collaboration is supported where each
  organization can apply its own design guidelines).
- OER should be defined in a lightweight markup language, which is easy
  to learn and which enables the use of industrial-strength version
  control systems such as Git for the management of OER collaboration
  (comparison, revision, merge).

Emacs-reveal meets all of the above requirements, which lowers entry
barriers towards a more widespread creation of OER.  Besides, it
simplifies licensing attribution when re-using figures with
machine-readable meta-data based on an extension of CC REL (The
Creative Commons Rights Expression Language) [@AAL+12], (a) avoiding
manual identification and copying of licensing information, which is
among the most time-consuming factors for OER projects [@FLGB16], and
(b) making licensing information accessible on the Semantic Web (with
RDFa in HTML) [@Hor08].

Source files for presentations are written in the lightweight
markup language Org Mode [@SD11] (which is native to the text editor
GNU Emacs [@emacs] but can be edited in any text editor) and converted
to slideshows based on the HTML presentation framework reveal.js
[@revealjs].  A Docker image for emacs-reveal
[@emacs-reveal-docker] can be used to generate OER presentations in
GitLab Continuous Integration infrastructures, notably a Howto
presentation [@howto] for emacs-reveal is maintained that way.


# Acknowledgements

The author acknowledges funding by the Ministry of Innovation, Science
and Research of the State of North Rhine-Westphalia, Germany, and
Stifterverband, Germany during the genesis of this project.

# References
