;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bartłomiej Świercz"
      user-mail-address "bartek@rndity.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka Term SS04" :size 13 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Term Slab" :size 13 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-vibrant)
;;(setq doom-theme 'doom-monokai-classic)
;;(setq doom-theme 'modus-vivendi)

(use-package! modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-hl-line '(intense)
        modus-themes-subtle-line-numbers t
        modus-themes-paren-match '(bold intense)
        modus-themes-region '(bg-only)
        modus-themes-scale-headings t
        modus-themes-mode-line '(borderless)
        modus-themes-bold-constructs t
        modus-themes-syntax '(faint alt-syntax green-strings)
        modus-themes-mixed-fonts t
        modus-themes-fringes '(intense)
        modus-themes-intense-markup t
        modus-themes-org-blocks '(tinted-background))
  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package! bespoke-themes
  :config
  ;; Set evil cursor colors
  (setq bespoke-set-evil-cursors t)
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  (setq bespoke-set-theme 'dark)
  ;; Load theme
  ;;(load-theme 'bespoke t)
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(global-visual-line-mode t)
(setq display-line-numbers-type t
      default-directory "~"
      browse-url-browser-function 'eww-browse-url)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; By default doom blacklists SSH_AUTH_SOCK and SSH_AUTH_PID variables, which means ssh agents don’t work.
(when noninteractive
  (add-to-list 'doom-env-whitelist "^SSH_"))

;; -------------------------------
;; Dired
;; -------------------------------
(after! dired
  (remove-hook 'dired-mode-hook 'dired-omit-mode)) ; Ensure dired-omit-mode is not started with dired. It hides some files transparently.

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

;; -------------------------------
;; Spelling
;; -------------------------------
(after! ispell
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US,pl_PL"
        ispell-personal-dictionary "~/.doom.d/hunspell_personal")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,pl_PL"))

(after! flyspell
  (setq flyspell-lazy-idle-seconds 2))

;; -------------------------------
;; web mode
;; -------------------------------
(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        js-indent-level 2
        typescript-indent-level 2
        json-reformat:indent-width 2
        css-indent-offset 2
        prettier-js-args '("--single-quote")))


;; -------------------------------
;; embark
;; -------------------------------

; TODO

;; -------------------------------
;; pdf-tools
;; -------------------------------

(use-package! pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))


;; -------------------------------
;; org-mode
;; -------------------------------

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/"
      org-agenda-files (directory-files-recursively org-directory "\\.org$")
      ;;org-roam-directory (concat org-directory "roam/")
      ;;org-roam-db-location (concat org-roam-directory ".org-roam.db")
      org-journal-dir  (concat org-directory "journal/")
      org-journal-date-prefix "#+TITLE: "
      org-journal-date-format "%Y-%m-%d, %A"
      org-journal-file-format "%Y-%m-%d_journal.org"
      deft-directory org-directory
      deft-extensions '("org" "txt" "md")
      deft-recursive t)

;; org-protocol:
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

(after! org-capture
  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates
               '("p" "Protocol" entry
                 (file+datetree "web_notes.org")
                 "* %(transform-square-brackets-to-round-ones \"%:description\") %?\n:PROPERTIES:\n:Link: %:link\n:When: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
                 :prepend t
                 :empty-lines 1
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+datetree "web_notes.org")
                 "* %(transform-square-brackets-to-round-ones \"%:description\") %?\n:PROPERTIES:\n:Link: %:link\n:When: %U\n:END:\n"
                 :prepend t
                 :empty-lines 1
                 :kill-buffer t)))

(after! org
  (setq org-log-done t
        org-log-into-drawer t
        org-journal-enable-agenda-integration t
        org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(after! org-noter
  (setq org-noter-notes-search-path org-directory
        org-noter-hide-other nil
        org-noter-separate-notes-from-heading t
        org-noter-always-create-frame nil)
  (map!
   :map org-noter-doc-mode-map
   :leader
   (:prefix-map ("n p" . "org-noter")
    :desc "Start session" "e" #'org-noter)
    :desc "Insert note" "i" #'org-noter-insert-note
    :desc "Insert precise note"  "p" #'org-noter-insert-precise-note
    :desc "Go to previous note" "k" #'org-noter-sync-prev-note
    :desc "Go to next note" "j" #'org-noter-sync-next-note
    :desc "Create skeleton" "s" #'org-noter-create-skeleton
    :desc "Kill session" "q" #'org-noter-kill-session))

;; -------------------------------
;; Google Translate
;; -------------------------------
(use-package google-translate
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))


;; -------------------------------
;; Edit with emacs Everywhere
;; https://github.com/dmgerman/editWithEmacs.spoon
;; -------------------------------
(load! "../.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon")
