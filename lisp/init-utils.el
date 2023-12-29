;;; init-utils.el --- Emacs lisp utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defconst mabo3n/home-dir (expand-file-name "~/")
  "User home directory (default ~/).")

(defun mabo3n/transform-strings (transformations strings &optional ignore-case msg)
  "Sequentially apply TRANSFORMATIONS in STRINGS.

STRINGS is a string or a list of strings, and TRANSFORMATIONS
is an association list with (REGEXP . REPLACEMENT) entries.

For each string in STRINGS, replace text matching each REGEXP
in TRANSFORMATION cells with its respective REPLACEMENT.

Replacements are performed with `replace-regexp-in-strings'.
The value of IGNORE-CASE is set to variable `case-fold-search'
before each replacement.

If MSG is non-nil, display a message indicating each transformed
string (if any)."
  (let* ((case-fold-search ignore-case)
         (transform-function
          (lambda (str)
            (let ((transformed
                   (cl-reduce (lambda (cur transformation)
                                (replace-regexp-in-string (car transformation)
                                                          (cdr transformation)
                                                          cur
                                                          t))
                              transformations
                              :initial-value str)))
              (and msg
                   (not (string-equal str transformed))
                   (message "%s -> %s" str transformed))
              transformed))))
    (if (stringp strings)
        (funcall transform-function strings)
      (mapcar transform-function strings))))

(defun mabo3n/url-decode-region (beg end &optional echo-only allow-newlines)
  "Replace URL between BEG and END with its unhexed version.

When called with a `\\[universal-argument]', or if ECHO-ONLY is
t, only print the decoded url in the minibuffer.
ALLOW-NEWLINES is passed along to `url-unhex-string'."
  (interactive "r\nP")
  (let* ((url         (buffer-substring-no-properties beg end))
         (decoded-url (url-unhex-string url allow-newlines)))
    (kill-new decoded-url)
    (if echo-only
        (message "%s" decoded-url)
      (delete-region beg end)
      (insert decoded-url))))

(defun mabo3n/url-encode-region (beg end &optional echo-only allowed-chars)
  "Replace URL between BEG and END with its hexified version.

When called with a `\\[universal-argument]', or if ECHO-ONLY is
t, only print the encoded url in the minibuffer.
ALLOWED-CHARS is passed along to `url-hexify-string'."
  (interactive "r\nP")
  (let* ((url         (buffer-substring-no-properties beg end))
         (encoded-url (url-hexify-string url allowed-chars)))
    (kill-new encoded-url)
    (if echo-only
        (message "%s" encoded-url)
      (delete-region beg end)
      (insert encoded-url))))

(provide 'init-utils)
;;; init-utils.el ends here
