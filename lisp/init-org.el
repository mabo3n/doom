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
        ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :clock-resume t)

        ("w" "Jobs")

        ("wc" "Company" entry
         (file+headline "~/docs/jobs/companies.org" "Companies")
         "* REVIEW %(mabo3n/org-capture-jobs-read-new-company
                    )%^{Source}p%^{Source_link|-}p
  :LOGBOOK:
  - State \"REVIEW\"     from              %U
  :END:"
         :immediate-finish t)

        ("wp" "Position" entry
         (file+function "~/docs/jobs/companies.org"
                        mabo3n/org-capture-jobs-position-find-headline-function)
         "* %^{Title} (%^{Minimum yoe|-}+ yoe)
  :PROPERTIES:
  :Yoe: %\\2
  :END:
  [[%^{Link}][Link]]%?"
         :jump-to-captured t)

        ("wa" "Application" entry
         (file+headline "~/docs/jobs/processes.org" "Processes")
         "* %(mabo3n/org-capture-template-read-headline-link
              \"~/docs/jobs/companies.org\"
              '((\"LEVEL=2+company\"))) / %(read-string
                                            \"Title: \")%^{Webpage}p%?"
         :immediate-finish t :jump-to-captured t)

        ("wi" "Interview" entry
         (file+function "~/docs/jobs/processes.org"
                        (lambda ()
                          (mabo3n/org-capture-dive-to-headline
                           '(("LEVEL=2+process" . "Application: ")))))
         "* Interview (%^{Type|Phone screen|Technical screen|Other}) :interview:
  %^T"
         :immediate-finish t :jump-to-captured t)

        ("ws" "Step" entry
         (file+function "~/docs/jobs/processes.org"
                        (lambda ()
                          (mabo3n/org-capture-dive-to-headline
                           '(("LEVEL=2+process" . "Application: ")))))
         "* %^{Step}
  DEADLINE: %^t"
         :immediate-finish t :jump-to-captured t)

      ))


(defun mabo3n/org-capture-find-file-function (file)
  "Find FILE to insert the capture template.

This mimics the \"file+headline\" option's default behavior
to find the file, found in `org-capture-set-target-location'."
  (set-buffer (org-capture-target-buffer file))
  (unless (derived-mode-p 'org-mode)
    (org-display-warning
     (format "Capture requirement: switching buffer %S to Org mode"
             (current-buffer)))
    (org-mode))
  (org-capture-put-target-region-and-position)
  (widen))

(defun mabo3n/org-capture-find-headline-function (headline &optional level)
  "Find (or create) HEADLINE to insert the capture template.

This mimics the \"file+headline\" option's default behavior
to find the headline, found in `org-capture-set-target-location'.

The default behavior is to try to find a headline at any level,
and to create a top (1) level headline if not found.

This function also accepts an optional LEVEL argument which,
if is a number, restrict the search to and use LEVEL for creation."
  (goto-char (point-min))
  (if (and (re-search-forward (format org-complex-heading-regexp-format
                                      (regexp-quote headline))
                              nil t)
           (or (and (numberp level) (= (org-outline-level) level))
               t))
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (concat (make-string (or level 1) ?*) " ") headline "\n")
    (beginning-of-line 0)))

(defun mabo3n/org-capture-read-headline (&optional match prompt allow-new)
  "Read a headline name matching MATCH in current buffer.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
PROMPT is the text of the completion. Defaults to \"Headline: \".
ALLOW-NEW if non-nil lets the user input any string, even if it doesn't
match an existing headline.

When called interactively, prompts for MATCH string first.
With a \\[universal-argument], ALLOW-NEW is t."
  (interactive "sMatch string: \ni\nP")
  (let ((headlines
         (org-map-entries (apply-partially 'org-entry-get nil "ITEM")
                          match))
        (prompt (or prompt "Headline: ")))
    (completing-read prompt headlines nil (not allow-new))))

(defun mabo3n/org-capture-dive-to-headline (match-sequence &optional allow-new)
  "Read a headline according to MATCH-SEQUENCE in current buffer.

This acts like a recursive version of `mabo3n/org-capture-read-headline',
where MATCH-SEQUENCE is a list of MATCH strings or (MATCH . PROMPT) entries.

For example:
  '(nil      ;; Prompt for any headline first
    (\"+country+tropics\" . \"Choose a country: \")
    (nil . \"Which state:\")
    \"+beach-cold\")

Note that headline search is performed by plain text.
If multiple headlines have the same text, the first one is chosen.
Point is left in the line of the last searched subheading.

ALLOW-NEW if non-nil lets the user input any string
as the last headline, even if it doesn't match an existing one.

When called interactively, prompts for MATCH-SEQUENCE string first.
With a \\[universal-argument], ALLOW-NEW is t."
  (interactive "xRaw match-sequence: \nP")
  (save-restriction
    (cl-loop
     with length = (length match-sequence)
     for entry in match-sequence
     for match = (if (consp entry) (car entry) entry)
     for prompt = (if (consp entry) (cdr entry) nil)
     for index from 1
     for lastp = (= index length)
     for headline = (mabo3n/org-capture-read-headline
                     match prompt (and lastp allow-new))
     do (mabo3n/org-capture-find-headline-function
         headline
         ;; FIXME if match-sequence has 1 entry and allow-new is
         ;; non-nil, "new" headings will be created with the level
         ;; of the entry under point when the function was called.
         (when lastp (1+ (org-outline-level))))
        (org-narrow-to-subtree))))

(defun mabo3n/org-capture-template-read-headline-link (file match-sequence)
  "Return a link to a headline read through MATCH-SEQUENCE in FILE.

This can be used in templates to expand to a link to a headline,
which is chosen interactively by a sequential of [parent] headline prompts
according to MATCH-SEQUENCE.
See function `mabo3n/org-capture-dive-to-headline' for more details."
  (with-current-buffer (org-capture-target-buffer file)
     (mabo3n/org-capture-dive-to-headline match-sequence)
     ;; apparently this doesn't work non-interactively
     ;; https://narkive.com/RtJ6Kjrt.1
     (call-interactively 'org-store-link))
  (with-temp-buffer
    (org-insert-last-stored-link 1)             ;; striping the \n
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

;;; org-capture / Jobs

(defun mabo3n/org-capture-jobs-read-new-company ()
  "Read a (new) company name for the Jobs / Company template."
  (with-current-buffer (org-capture-target-buffer
                        "~/docs/jobs/companies.org")
    (mabo3n/org-capture-read-headline
     "LEVEL=2+company" "Company name: " t)))

(defun mabo3n/org-capture-jobs-position-find-headline-function ()
  "Find location function for Jobs / Position capture."
  (mabo3n/org-capture-dive-to-headline '(("LEVEL=2+company" . "Companies: ")))
  (save-restriction
    (org-narrow-to-subtree)
    (mabo3n/org-capture-find-headline-function "Positions" 3)
    (org-set-tags ":position:")))

(provide 'init-org)
;;; init-org.el ends here
