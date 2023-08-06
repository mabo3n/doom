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
(setq doom-theme 'doom-one)

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

(setq doom-theme 'doom-nord-light)
;; (doom/reset-font-size)
;; (doom/increase-font-size 2)

(use-package! which-key
  :init (setq which-key-idle-delay 0.15))

(map! :leader "SPC" nil) ;; #'execute-extended-command)

(after! vertico
  (map! :map vertico-map
        ;; C-SPC for embark-act
        "C-SPC"   #'embark-act
        "C-S-SPC" #'embark-act-all
        "C-;"     #'+vertico/embark-preview ;; just in case
        ;; Emacs-like file path navigation
        "C-l" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
        "C-j" (cmds! (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-preview
                     #'+vertico/embark-preview))
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
  (setq embark-help-key "C-SPC"))

(after! company
  (map! :map company-active-map
        "C-h" #'company-show-location
        "C-j" #'company-show-doc-buffer))

;; (after! embark
;;   (map! :map embark-general-map
;;         ;; Swap SPC and C-SPC in embark keymaps
;;         :desc "<leader>" "C-SPC" #'embark-select
;;         :desc "mark"    "SPC"   #'mark
;;         ))

(after! dired
  (map! :leader "f j" #'dired-jump))

(setq display-line-numbers-type nil)

;; TODO Find alternative to symbol highlight and edit (SPC s e)
