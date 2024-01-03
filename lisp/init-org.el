;;; init-org.el --- Org stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)

(setq-default
 org-cycle-separator-lines 2
 org-refile-allow-creating-parent-nodes 'confirm
 org-log-into-drawer t
 ;; org-adapt-indentation t  ;; FIXME This seems to be happening already?
 ;; org-refile-targets '((nil :maxlevel . 1))
 org-id-link-to-org-use-id 'use-existing
 org-startup-folded 'show2levels
 ;; org-hide-block-startup t
 ;; org-hide-emphasis-markers t
 org-image-actual-width nil)

(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

(setq-default
 org-archive-location ".archive.%s::"
 org-archive-file-header-format
 (concat ";;; -*- mode: org; -*-\n"
         "Archived entries from file =%s=:\n"
         "\n"))

(provide 'init-org)
;;; init-org.el ends here
