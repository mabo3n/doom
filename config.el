;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(defconst mabo3n/workp t
  "Non nil if I'm in my work setup")

;; (doom/reset-font-size)
;; (doom/increase-font-size 2)

(use-package! which-key
  :init (setq which-key-idle-delay 0.4))

(map! :leader "SPC" nil) ;; #'execute-extended-command)

(setq show-trailing-whitespace t)

(after! vertico
  (map! :map vertico-map
        ;; C-SPC for embark-act
        "C-SPC"   #'embark-act
        "C-S-SPC" #'embark-act-all
        "C-;"     #'+vertico/embark-preview ;; just in case
        ;; Emacs-like file path navigation
        "C-l" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
        "C-j" (cmds! (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-preview
                     #'+vertico/embark-preview)
        ;; C-n and C-p are bound to next/prev by default but these are not
        "C-M-n"   #'vertico-next-group
        "C-M-p"   #'vertico-previous-group)
  (setq vertico-cycle nil
        vertico-count 11))

(after! consult
  ;; Overriding config to use C-j as preview key instead of C-SPC
  (setq consult--customize-alist nil)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-j")
  (consult-customize
   consult-theme
   :preview-key (list "C-j" :debounce 0.5 'any)))

(after! embark
  (setq embark-help-key "C-SPC"
        embark-quit-after-action nil))

(after! company
  (map! :map company-active-map
        "C-h" #'company-show-location
        "C-j" #'company-show-doc-buffer))

;; Restore `s' to yank surround instead of evil snipe
(when (modulep! :editor evil +everywhere)
  (remove-hook 'doom-first-input-hook #'evil-snipe-mode)
  (map! :v "S" nil ;; this one is not working dunno why
        :v "s" #'evil-surround-region))

;; Don't prompt if commit summary line is "too long"
(after! git-commit
  (setq git-commit-style-convention-checks (cl-remove 'overlong-summary-line
                                             git-commit-style-convention-checks)))

(when mabo3n/workp
  (after! browse-at-remote
    ;; Teach browse-at-remote to handle custom ssh host
    ;; of my private doom config
    (add-to-list 'browse-at-remote-remote-type-regexps
                 `(:host ,(rx bol "doom-cfg-host" eol)
                   :type "github"
                   :actual-host "github.com"))
    ;; HACK browse-at-remote to generate valid URLs to Azure repos
    (add-to-list 'browse-at-remote-remote-type-regexps
                 `(:host ,(rx bol "ssh.dev.azure.com" eol)
                   :type "ado"
                   :actual-host "dev.azure.com"))

    (defadvice! mabo3n/fix-browse-at-remote-ado-url-a (fn &rest args)
      "Append organization to host to generated URLs."
      :around #'browse-at-remote-ado-format-url
      (s-replace "//dev.azure.com"
                 "//dev.azure.com/AMBEV-SA"
                 (apply fn args)))))

(after! dired
  (map! :leader "f j" #'dired-jump))

(map! :when (modulep! :ui workspaces)
      :leader
      :desc "Restore last session" "TAB r"   #'+workspace/restore-last-session
      :desc "Rename workspace"     "TAB R"   #'+workspace/rename
      :desc "Switch workspace"     "TAB TAB" #'+workspace/switch-to
      :desc "Display tab bar"      "TAB ."   #'+workspace/display)

(setq display-line-numbers-type nil)

;; C# stuff

;; Make eglot know that csharp-tree-sitter-mode is also csharp.
;; Find the csharp entry in alist and add new mode there
(when (and (modulep! :tools lsp +eglot)
           (modulep! :lang csharp +lsp))
  (after! eglot
    (let* ((csharp-entry (assoc 'csharp-mode
                                eglot-server-programs
                                (lambda (entry mode)
                                  (and (listp entry)
                                       (cl-member mode entry)))))
           (mode 'csharp-tree-sitter-mode))
      (unless (cl-member mode (car csharp-entry))
        (push mode (car csharp-entry))))))

;; org stuff

(when (modulep! :lang org)
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
           "\n")))

;; org-roam stuff

(when (modulep! :lang org +roam2)
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
  (add-hook 'before-save-hook #'mabo3n/org-roam-timestamp-on-save))

;; biblio stuff

(when (modulep! :tools biblio)
  (setq org-cite-global-bibliography '("~/docs/My Library.bib")
        citar-bibliography org-cite-global-bibliography))
