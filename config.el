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

(defconst mabo3n/home-dir (expand-file-name "~/")
  "User home directory (default ~/).")

(defconst mabo3n/workp nil
  "Non nil if I'm in my work setup")

(load! "lisp/init-utils")
(load! "lisp/init-backup")

(map! :leader
      "f : b" #'mabo3n/backup-file)

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

(map! :leader
      :desc "Switch to last buffer" "b `" #'evil-switch-to-windows-last-buffer)

(map! :when (modulep! :ui workspaces)
      :leader
      :desc "Restore last session" "TAB r"   #'+workspace/restore-last-session
      :desc "Rename workspace"     "TAB R"   #'+workspace/rename
      :desc "Switch workspace"     "TAB TAB" #'+workspace/switch-to
      :desc "Display tab bar"      "TAB ."   #'+workspace/display)

(setq display-line-numbers-type nil
      avy-timeout-seconds 0.25)

;; Python stuff

(after! python
  (map! :map python-mode-map
        :nvi "C-<return>" #'python-shell-send-statement))

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
  (load! "lisp/init-org"))

(when (modulep! :lang org +roam2)
  (load! "lisp/init-org-roam"))

;; biblio stuff

(when (modulep! :tools biblio)
  (setq org-cite-global-bibliography '("~/docs/My Library.bib")
        citar-bibliography org-cite-global-bibliography))
