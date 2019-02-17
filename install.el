;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; If you use the Docker image specified in .gitlab-ci.yml,
;; this file is not necessary as the image contains the packages
;; installed here.

(defun install ()
  "Install prerequisites from MELPA"
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-refresh-contents)
  (package-install 'org-re-reveal-ref)
  )
