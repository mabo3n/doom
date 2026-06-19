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

;; Default babel results to stdout + verbatim (terminal-like behavior).
;; No more automatic table coercion — output is inserted as-is.
(setq org-babel-default-header-args
      (assq-delete-all :results org-babel-default-header-args))
(add-to-list 'org-babel-default-header-args '(:results . "output verbatim replace"))

(use-package! ox-gfm
  :after org)

;;; org-link abbreviations

(dolist (abbr '(("coppola"  . "https://backoffice.nubank.com.br/coppola/#/customers/%s")
                ("shuffle"  . "https://backoffice.nubank.com.br/shuffle/#/person/%s")
                ("tristram" . "https://backoffice.nubank.com.br/tristram/#/customer/%s")
                ("cia"      . "https://backoffice.nubank.com.br/cia/user-lookup/?idType=customer_id&idValue=%s")))
  (add-to-list 'org-link-abbrev-alist abbr))

;;; org-capture

(defvar mabo3n/org-capture-work-item-label "Work item"
  "Work items label for capturing.")
(defvar mabo3n/org-capture-work-items-dir-path
  (expand-file-name "work-items/" mabo3n/zone-dir)
  "Base path for work items' directories.")
(defvar mabo3n/org-capture-work-item-last-id nil
  "Last prompted work item ID.")
(defvar mabo3n/org-capture-work-item-default-id-prefix "LPIP-"
  "Default ID prefix prompted when capturing a work item.")

(defun mabo3n/org-capture-work-item-get-create-path ()
  "Read work item ID and return proper file path.

If work item was already captured, open its main file instead.
If `current-prefix-arg', `dired' to `mabo3n/org-capture-work-items-dir-path'."
  (if current-prefix-arg
      (and (dired mabo3n/org-capture-work-items-dir-path)
           nil)
    (let* ((base-dir-path mabo3n/org-capture-work-items-dir-path)
           (existing-work-items (when (file-directory-p base-dir-path)
                                  (directory-files base-dir-path t)))
           (selected-path (completing-read
                           (concat mabo3n/org-capture-work-item-label ": ")
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 '(metadata (category . file))
                               (complete-with-action action existing-work-items string pred)))
                           nil nil mabo3n/org-capture-work-item-default-id-prefix))
           (id (if (file-name-absolute-p selected-path)
                   (file-name-nondirectory (directory-file-name selected-path))
                 selected-path))
           (dir-path (expand-file-name (concat id "/")
                                       mabo3n/org-capture-work-items-dir-path))
           (file-path (concat dir-path id ".org")))

      (setq mabo3n/org-capture-work-item-last-id id)

      ;; Just open the file if it's already there
      (if (file-exists-p file-path)
          (progn
            (find-file file-path)
            ;; Return nil to tell org-capture to stop
            (error "%s already exists! There you have it" id))
        file-path))))

(defun mabo3n/org-capture-ops-ticket-get-create-path ()
  "Like `mabo3n/org-capture-work-item-get-create-path' but default to LEO- prefix."
  (let ((mabo3n/org-capture-work-item-default-id-prefix "LEO-"))
    (mabo3n/org-capture-work-item-get-create-path)))

(setq org-capture-templates
      `(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :clock-resume t :kill-buffer t)
        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :clock-resume t :kill-buffer t)

        ("#" "Work items")
        ("#i" ,mabo3n/org-capture-work-item-label plain
         (file mabo3n/org-capture-work-item-get-create-path)
         ,(concat ":PROPERTIES:\n"
                  ":DIR: ./\n"
                  ":END:\n\n"
                  "#+TITLE: %(append mabo3n/org-capture-work-item-last-id)\n"
                  "#+ROAM_REFS: https://nubank.atlassian.net/browse/%(append mabo3n/org-capture-work-item-last-id)\n"
                  "#+OPTIONS: toc:nil num:nil author:nil\n\n%?")
         :immediate-finish nil :jump-to-captured t)
        ("#t" "ops Ticket" plain
         (file mabo3n/org-capture-ops-ticket-get-create-path)
         ,(concat ":PROPERTIES:\n"
                  ":DIR: ./\n"
                  ":customer_id: %^{customer_id}\n"
                  ":END:\n\n"
                  "#+TITLE: %(append mabo3n/org-capture-work-item-last-id)\n"
                  "#+ROAM_REFS: https://nubank.atlassian.net/browse/%(append mabo3n/org-capture-work-item-last-id)\n"
                  "#+OPTIONS: toc:nil num:nil author:nil\n"
                  "#+PROPERTY: header-args:sh :prologue \"export NU_COUNTRY=br\"\n"
                  "#+PROPERTY: header-args+ :var customer_id=\"%\\1\" :var shard=\"%^{shard}\"\n\n"
                  "🔗 [[coppola:%\\1][Coppola]] / [[shuffle:%\\1][Shuffle]] / [[tristram:%\\1][Tristram]] / [[cia:%\\1][CIA]]\n\n%?")
         :immediate-finish nil :jump-to-captured t)

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

;; Resetting `[ ]' keybindings to match the char inside the brackets
(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence
           "[ ](T)" "[-](-)" "[?](?)" "|" "[X](x)")
          (sequence
           "|" "OKAY(o)" "YES(y)" "NO(n)"))))

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
