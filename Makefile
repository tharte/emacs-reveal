# SPDX-FileCopyrightText: 2020 Jens Lechtenbörger
# SPDX-License-Identifier: CC0-1.0

BUILDHTML := emacs --batch --load elisp/publish.el
GITTAG    := $(shell git describe --tags)
TARFILE   := emacs-reveal.tar.bz2
TAROPTS   := --exclude-vcs --exclude=emacs-reveal.tar* -cvjf

.PHONY: archive docker html init setup

html: setup
	rm -rf ~/.org-timestamps
	cd tests && $(BUILDHTML) && cd ..

init:
	git submodule sync --recursive
	git submodule update --init --recursive

setup: init
	cd org-mode && make clean && make autoloads && cd ..

tar:
	tar $(TAROPTS) docker/$(TARFILE) .

archive: setup tar

docker: archive
	docker build -t emacs-reveal:$(GITTAG) -f docker/emacs-reveal/Dockerfile docker
