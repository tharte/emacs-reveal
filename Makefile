# SPDX-FileCopyrightText: 2020 Jens Lechtenbörger
# SPDX-License-Identifier: CC0-1.0

DIR := ${CURDIR}

BUILDHTML := emacs --batch --load elisp/publish.el
GITTAG    := $(shell git describe --tags)
TARFILE   := emacs-reveal.tar.gz
TAROPTS   := --exclude-vcs --exclude=emacs-reveal.tar* --exclude=./tests -cvzf
TESTDIR   := $(DIR)/tests

ROBOT_FRAMEWORK    := ppodgorsek/robot-framework:latest
ROBOT_HTML_DIR     := $(TESTDIR)/public
ROBOT_REPORTS_DIR  := $(TESTDIR)/reports
ROBOT_TESTS_DIR    := $(TESTDIR)/robotframework

.PHONY: all archive docker html init robot-test setup tar

all: html archive

html: setup
	rm -rf ~/.org-timestamps
	cd $(TESTDIR) && $(BUILDHTML) && cd $(DIR)

init:
	git checkout master
	git pull
	git submodule sync --recursive
	git submodule update --init --recursive

setup: init
	cd org-mode && make clean && make autoloads && cd $(DIR)

tar:
	tar $(TAROPTS) docker/$(TARFILE) .

archive: setup tar

docker: archive
	docker build -t emacs-reveal:$(GITTAG) -f docker/emacs-reveal/Dockerfile docker

robot-test:
	docker run -v $(ROBOT_REPORTS_DIR):/opt/robotframework/reports:Z -v $(ROBOT_TESTS_DIR):/opt/robotframework/tests:Z -v $(ROBOT_HTML_DIR):/robot/public -e BROWSER=${BROWSER} -e PRESENTATION=${PRESENTATION} $(ROBOT_FRAMEWORK)
