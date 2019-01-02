;; If you use the Docker image specified in .gitlab-ci.yml,
;; this file is not necessary as the image contains the packages
;; installed here.

(defun install ()
  "Install prerequisites from ELPA"
  (package-initialize)
  ;; org-ref is available on melpa:
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-refresh-contents)
  ;; Emacs comes with an outdated version of org.
  ;; Need to be more specific to install newer version:
  (package-install (cadr (assq 'org package-archive-contents)))
  ;; (package-install 'htmlize) ; Now a dependency of org-ref
  (package-install 'org-ref)
  )
