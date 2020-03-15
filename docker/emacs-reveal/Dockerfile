# SPDX-FileCopyrightText: 2019-2020 Jens Lechtenbörger
# SPDX-License-Identifier: CC0-1.0

# Package ditaa requires OpenJDK, which has a known bug, which in turn
# requires the mkdir command below:
# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199

FROM registry.gitlab.com/oer/emacs-reveal/debian-emacs-tex
LABEL maintainer="Jens Lechtenbörger"
RUN mkdir -p /usr/share/man/man1 && \
    apt-get update -y && apt-get --no-install-recommends install -y \
    curl ditaa ghostscript graphviz imagemagick
ENV PU_VERSION 1.2020.2
RUN curl --create-dirs -o /root/plantuml-$PU_VERSION/plantuml.zip -L https://sourceforge.net/projects/plantuml/files/$PU_VERSION/plantuml-jar-lgpl-$PU_VERSION.zip/download \
    && cd /root/plantuml-$PU_VERSION && unzip plantuml.zip && rm plantuml.zip
COPY code/* /tmp/
RUN cp /tmp/.emacs /root/
RUN /tmp/install-melpa.sh "org-ref"
ADD emacs-reveal.tar.gz /root/.emacs.d/elpa/emacs-reveal
