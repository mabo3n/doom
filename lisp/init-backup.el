;;; init-backup.el --- Custom backup functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)
;; (require 'subr-x)
;; (require 'dash)
;; (require 'simple)
;; (require 'core-load-paths)
;; (require 'core-dotspacemacs)
;; (require 'helm)

(defconst mabo3n/backup-files-remote-root "d:"
  "Where `mabo3n/backup-file' moves files to.")

(defconst mabo3n/backup-files-default-args
  (list "-L" "--exclude=\".DS_Store\"")
  "Default args used by `mabo3n/backup-file'.")

(defconst mabo3n/backup-files-path-transformations
  `(("^Documents" . "docs")
    ("^Downloads" . "down")
    ("^Pictures"  . "pics")
    ("^Desktop"   . "desk")
    ("^\\."       . ,(concat (file-name-as-directory "dot") ".")))
  "File path transformations to be applied before backing up files.

Each entry has form (REGEXP . REPLACEMENT), where REGEXP is the
remote path sans the initial `mabo3n/backup-files-remote-root',
and REPLACEMENT the replacement string to substitute REGEXP.")

(defun mabo3n/backup-files--get-remote-path (file)
  "Get remote destination for local FILE.

Apply each of `mabo3n/backup-files-path-transformations' on FILE,
and return its remote destination, which basically is \"the path
to the last directory on FILE, relative to `mabo3n/home-dir',
prepended by `mabo3n/backup-files-remote-root'\".

Note that no sanitization/validation against paths are performed."
  (let ((expanded-file (expand-file-name file)))
    (when (string-prefix-p mabo3n/home-dir expanded-file)
      (let* ((relative-file-path
              (string-trim-left expanded-file
                                (concat "^" (file-name-as-directory
                                             mabo3n/home-dir))))
             (dirp (file-directory-p expanded-file))
             (transformed-path
              (mabo3n/transform-strings mabo3n/backup-files-path-transformations
                                        relative-file-path))
             (transformed-path-last-directory
              (if dirp
                  ;; **/foo/dir -> **/foo/dir/
                  (file-name-as-directory transformed-path)
                ;; **/foo/file -> **/foo/
                (file-name-directory transformed-path))))
        (concat mabo3n/backup-files-remote-root
                transformed-path-last-directory)))))

(defun mabo3n/backup-files--build-backup-command (file &optional args)
  "Generate a shell command to backup FILE.

FILE may by a file path or a list of file paths.
ARGS is a list of string arguments forwarded to rclone."
  (let ((command
         (catch 'continue
           (let* ((f (or (and (consp file) (car file))
                         file))
                  (expanded-file
                   (if (stringp f) (expand-file-name f)
                     (message "File is not a string \"%s\"." f)
                     (throw 'continue nil)))
                  (destination-file
                   (cond
                    ((not (string-prefix-p mabo3n/home-dir expanded-file))
                     (message "Cannot backup file from outside user home directory \"%s\"."
                              expanded-file)
                     (throw 'continue nil))
                    ((string= mabo3n/home-dir expanded-file)
                     (message "Cannot backup the whole user home directory \"%s\"."
                              expanded-file)
                     (throw 'continue nil))
                    (t (mabo3n/backup-files--get-remote-path expanded-file)))))
             (unless destination-file
               (message "Couldn't get remote path for \"%s\"" expanded-file)
               (throw 'continue nil))
             (concat (format "rclone copy \"%s\" \"%s\""
                             expanded-file
                             destination-file)
                     (and args " ")
                     (string-join args " "))))))
    (if (consp file)
        (let ((fn (apply-partially #'mabo3n/backup-files--build-backup-command
                                   (cdr file) args)))
          (if command
              (cons command (and (cdr file) (funcall fn)))
            (funcall fn)))
      command)))

(defun mabo3n/backup-files--run-command (command)
  "Run COMMAND in a dedicated buffer, printing the exit status when done."
  (let ((buf "*Backup files*"))
    (with-current-buffer-window buf nil nil
      (async-shell-command command buf buf)
      (insert command)
      (shell-mode)
      (evil-normal-state)
      (switch-to-buffer-other-window buf)
      (let ((process (get-buffer-process buf)))
        (set-process-sentinel
         process
         (lambda (_ signal)
           (when (memq (process-status process) '(exit signal))
             (with-current-buffer buf
               (goto-char (point-max))
               (let ((inhibit-read-only t))
                 (insert "\n" (substring signal 0 -1) ".")
                 (help-mode))))))))))

(defun mabo3n/backup-file (file &optional args)
  "Upload FILE to cloud under `mabo3n/backup-files-remote-root'.

This builds and executes an rclone's copy command for FILE using ARGS.

FILE can be a file path or list of file paths.
ARGS is a list of args. If nil, defaults to
`mabo3n/backup-files-default-args'.

When called interactively, prompts for a file and use default ARGS.
With a `\\[universal-argument]', also prompts for an argument string.

Backing up files outside of, or the whole `mabo3n/home-dir',
is not allowed (they are ignored).

See URL `https://rclone.org/commands/rclone_copy/' for more info
about rclone's copy command behavior."
  (interactive "i\nP")
  (let* ((file (or file
                   (and (called-interactively-p 'any)
                        (helm-read-file-name
                         "File: "
                         :initial-input (or (dired-get-filename nil t)
                                            (buffer-file-name)
                                            default-directory)))))
         (default-args mabo3n/backup-files-default-args)
         (args (or (and (consp current-prefix-arg)
                        (list (read-string
                               "args: "
                               (string-join default-args " "))))
                   default-args))
         (command (-> (if (listp file) file (list file))
                      (mabo3n/backup-files--build-backup-command args)
                      (string-join ";\n"))))
    (mabo3n/backup-files--run-command command)))

(defun mabo3n/backup-recent-files (files &optional args)
  "Upload recent (24h) edited FILES to cloud.

Uses `mabo3n/backup-file' with ARGS."
  (mabo3n/backup-file files (append args '("--max-age 24"))))

(defun mabo3n/backup-recent-dotfiles (&optional args)
  "Upload recent (24h) edited dotfiles to cloud.

\".gitconfig\"
\".bashrc\" \".bash_profile\" \".bash_aliases\"
\".config/\" \".vimrc\"

Uses `mabo3n/backup-recent-files' with ARGS."
  (interactive)
  (mabo3n/backup-recent-files
   (mapcar (apply-partially 'concat (file-name-as-directory
                                     mabo3n/home-dir))
           '(".gitconfig"
             ".bashrc" ".bash_profile" ".bash_aliases"
             ".config/" ".vimrc"))
   args))

(defun mabo3n/backup-org-files (&optional args)
  "Upload org files to cloud.

Uses `mabo3n/backup-recent-files' with ARGS."
  (interactive)
  (mabo3n/backup-file
   `(,(expand-file-name "org/" mabo3n/home-dir)) args))

(defconst mabo3n/backup-dotspacemacs-default-commit-message
  "<auto commit>"
  "Default commit message used by `mabo3n/backup-dotspacemacs-changes'.")

(defun mabo3n/backup-dotspacemacs-files (&optional commit-msg)
  "Commit and push to remote all dotspacemacs file changes.

Optional COMMIT-MSG can be provided, using
`mabo3n/backup-dotspacemacs-default-commit-message' as the default one."
  (interactive (list
                (read-string
                 (format "Commit message (default %s):\n"
                         mabo3n/backup-dotspacemacs-default-commit-message))))
  (let* ((default-directory dotspacemacs-directory)
         (msg (or commit-msg
                  mabo3n/backup-dotspacemacs-default-commit-message))
         (command (format (concat "git add ."
                                  " && git commit -m \"%s\""
                                  " && git push")
                          msg)))
    (mabo3n/backup-files--run-command command)))

(spacemacs/declare-prefix "o b" "mabo3n/backup")

(spacemacs/set-leader-keys
  "o b f" #'mabo3n/backup-file
  "o b o" #'mabo3n/backup-org-files
  "o b d" #'mabo3n/backup-recent-dotfiles
  "o b C-c" #'mabo3n/backup-dotspacemacs-files)

(provide 'init-backup)
;;; init-backup.el ends here
