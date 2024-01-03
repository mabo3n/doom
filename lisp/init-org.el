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

;;; org-capture

(setq org-capture-templates
      '(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("p" "Templates for projects")
        ("pt" "Project-local todo" entry
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("pn" "Project-local notes" entry
         (file+headline +org-capture-project-notes-file "Inbox")
         "* %U %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("pc" "Project-local changelog" entry
         (file+headline +org-capture-project-changelog-file "Unreleased")
         "* %U %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("o" "Centralized templates for projects")
        ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :clock-resume nil)
        ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :clock-resume t :kill-buffer t)
        ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :clock-resume t)))

(provide 'init-org)
;;; init-org.el ends here
