;;; git-invoke-ert-tests.el --- Tests for git-invoke  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2020 Jens Lechtenbörger

;;; Commentary:
;; Beware!  This downloads lots of data (100 MB)...
;; Run tests as follows in batch mode (tests require "/" as path separator):
;; emacs --batch -L . -l ert -l git-invoke-ert-tests.el -f ert-run-tests-batch-and-exit

;;; Code:
(require 'git-invoke)

(ert-deftest test-emacs-reveal ()
  (let* ((tmpdir (make-temp-file "ert" t))
         (url "https://gitlab.com/oer/emacs-reveal.git")
         (version "5.0.0")
         (gitdir (concat (file-name-as-directory tmpdir) "emacs-reveal"))
         (gitsubdir (concat gitdir "/LICENSES"))
         (subdir (concat gitdir "/org-re-reveal"))
         (testfile (concat subdir "/org-re-reveal.el"))
         (testdir (concat subdir "/test-cases")))
    ;; Clone repo.
    (git-invoke-clone tmpdir url)

    ;; Cloned tag is newer than variable version.  Test checkout.
    (should-not (string= (git-invoke-get-tag gitdir) version))
    (git-invoke-checkout gitdir version)
    (should (string= (git-invoke-get-tag gitdir) version))

    ;; Clone does not populate submodules.  Test submodule update.
    (should-not (file-readable-p testfile))
    (git-invoke-update-submodules gitdir)
    (should (file-readable-p testfile))

    ;; Misc.
    (should (string-prefix-p (git-invoke-get-origin-url gitsubdir) url))
    (should (string= (git-invoke-get-toplevel testdir) subdir))

    ;; Pull returns to master, latest version.
    (git-invoke-pull gitdir)
    (should-not (string= (git-invoke-get-tag gitdir) version))

    (delete-directory tmpdir t)))
;; git-invoke-ert-tests.el ends here
