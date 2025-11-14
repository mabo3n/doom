;;; init-org-roam.el --- Org-roam stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(map! :leader
      :desc "Find node"        "n r n" #'org-roam-node-find
      :desc "Capture to node"  "n r c" #'org-roam-capture
      :desc "Add alias"        "n r a" #'org-roam-alias-add
      :desc "Remove alias"     "n r A" #'org-roam-alias-remove
      :desc "Add tag"          "n r t" #'org-roam-tag-add
      :desc "Remove tag"       "n r T" #'org-roam-tag-remove
      :desc "Open random node" "n r #" #'org-roam-node-random)

(setq org-roam-directory (expand-file-name "~/org/roam/")
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
(add-hook 'before-save-hook #'mabo3n/org-roam-timestamp-on-save)
