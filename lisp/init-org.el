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

(after! org-agenda
  (map! :map evil-org-agenda-mode-map :after evil-org-agenda
        ;; Reverse RET and S-RET in org-agenda
        :m "<return>"   #'org-agenda-goto
        :m "S-<return>" #'org-agenda-switch-to)
  ;; and narrow to heading after navigating
  (add-hook 'org-agenda-after-show-hook #'org-narrow-to-subtree))


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

;; org-roam stuff

(when (modulep! :lang org +roam)
  (map! :leader
        :desc "Find node"        "n r n" #'org-roam-node-find
        :desc "Capture to node"  "n r c" #'org-roam-capture
        :desc "Add alias"        "n r a" #'org-roam-alias-add
        :desc "Remove alias"     "n r A" #'org-roam-alias-remove
        :desc "Add tag"          "n r t" #'org-roam-tag-add
        :desc "Remove tag"       "n r T" #'org-roam-tag-remove
        :desc "Open random node" "n r #" #'org-roam-node-random)

  (setq org-roam-directory (expand-file-name "roam/" org-directory)
        ;; override default template to add created/modified/filetags props
        org-roam-capture-templates
        `(("d" "default" plain "\n* TODO roam entry: ${title}%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              ,(concat "#+title: ${title}\n"
                                       "#+created:       %U\n"
                                       "#+last_modified: %U\n"
                                       "#+filetags:\n"))
           :unnarrowed t)))

  ;; Update "last_modified" date when saving buffer
  ;; https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321
  (defun mabo3n/org-roam-set-time-stamp-vars ()
    "Set time-stamp variables to auto update last_modified property."
    (when (derived-mode-p 'org-mode)
      (require 'time-stamp)
      (setq-local time-stamp-active t
                  time-stamp-line-limit 24
                  time-stamp-start "#\\+last_modified:[ ]*"
                  time-stamp-end "$"
                  time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")))

  (defun mabo3n/org-roam-timestamp-on-save ()
    "Call `time-stamp' function if in `org-mode'."
    (when (derived-mode-p 'org-mode)
      (time-stamp)))
  (add-hook 'org-mode-hook #'mabo3n/org-roam-set-time-stamp-vars)
  (add-hook 'before-save-hook #'mabo3n/org-roam-timestamp-on-save))


(provide 'init-org)
;;; init-org.el ends here
