;;; init-nu-git-dlp.el --- interactive exemptions for Nubank's git-dlp hook -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defvar mabo3n/git-dlp-never-auto-exempt-policies '("CredentialPolicy")
  "Policies never offered for interactive exemption (always real, never a fluke).")

(defun mabo3n/git-dlp--parse-violations (output)
  "Parse Angelito's blocked-commit OUTPUT into (POLICY FILE LINE CONTENT) entries.
LINE and CONTENT are nil when Angelito reports a binary match (path only)."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (let (entries)
      (while (re-search-forward "^  Policy name: \\(.+\\)$" nil t)
        (let ((policy (string-trim (match-string 1))))
          (when (re-search-forward "^  Found in: *$" nil t)
            (forward-line 1)
            (while (looking-at "^    \\(\\S-+?\\):\\([0-9]+\\)\t\t\\(.*\\)$")
              (push (list policy (match-string 1) (match-string 2) (match-string 3)) entries)
              (forward-line 1))
            (while (looking-at "^    \\(\\S-+\\)$")
              (push (list policy (match-string 1) nil nil) entries)
              (forward-line 1)))))
      (nreverse entries))))

(defun mabo3n/git-dlp--group-by-file (entries)
  "Group ENTRIES from `mabo3n/git-dlp--parse-violations' by (POLICY . FILE)."
  (let (groups)
    (dolist (entry entries)
      (cl-destructuring-bind (policy file line content) entry
        (let* ((key (cons policy file))
               (cell (assoc key groups)))
          (if cell
              (push (cons line content) (cdr cell))
            (push (cons key (list (cons line content))) groups)))))
    (nreverse groups)))

(defun mabo3n/git-dlp--show-violation (policy file snippets)
  "Pop a buffer with SNIPPETS ((LINE . CONTENT)...) flagged in FILE by POLICY."
  (let ((buf (get-buffer-create "*git-dlp-violation*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Policy: %s\nFile:   %s\n\n" policy file))
      (dolist (snippet (reverse snippets))
        (insert (if (car snippet)
                    (format "  L%s: %s\n" (car snippet) (cdr snippet))
                  "  (binary match, no text snippet)\n"))))
    (display-buffer buf)))

(defun mabo3n/git-dlp-interactive-exempt (output)
  "Review Angelito violations in OUTPUT one by one, offering `nu gitdlp exempt'.
Returns non-nil (retry the commit) if at least one exemption was registered."
  (let ((groups (mabo3n/git-dlp--group-by-file (mabo3n/git-dlp--parse-violations output)))
        (exempted nil))
    (if (null groups)
        (message "%s" output)
      (dolist (group groups)
        (let* ((policy (caar group)) (file (cdar group)) (snippets (cdr group)))
          (if (member policy mabo3n/git-dlp-never-auto-exempt-policies)
              (message "git-dlp: %s flagged `%s' — refusing to auto-exempt, see /usr/local/git-dlp/AGENTS.md §3" policy file)
            (mabo3n/git-dlp--show-violation policy file snippets)
            (when (y-or-n-p (format "git-dlp: exempt `%s' from %s? " file policy))
              (let ((justification (read-string "Justification: "
                                                 (format "Excalidraw diagram artifact wrongly flagged by %s" policy))))
                (with-temp-buffer
                  (if (zerop (call-process "nu" nil t nil "gitdlp" "exempt" file policy justification))
                      (setq exempted t)
                    (message "git-dlp: exempt rejected for %s/%s:\n%s" policy file (buffer-string)))))))))
      (when (get-buffer "*git-dlp-violation*")
        (kill-buffer "*git-dlp-violation*")))
    (when exempted
      (magit-call-git "add" ".dlp-ignore.json"))
    exempted))

(provide 'init-nu-git-dlp)
;;; init-nu-git-dlp.el ends here
