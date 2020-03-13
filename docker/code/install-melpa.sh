#!/bin/sh
# Copyright (C) 2019 Jens Lechtenbörger
# SPDX-License-Identifier: CC0-1.0

if test -z "$1"
then
    echo "Supply name of MELPA package as argument!"
    exit 1
fi

emacs --batch --load /tmp/manage-packages.el \
    --eval="(mp-install-pkgs '($1) \"/tmp/archives\")"
