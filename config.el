;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Globals & defaults
;; -------------------------------

(setq user-full-name "Bartłomiej Świercz"
      user-mail-address "bartek@rndity.com")

(setq doom-font (font-spec :family "Iosevka Term SS04" :size 14 :weight 'light)
      doom-big-font (font-spec :family "Iosevka Term SS04" :size 20 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Term Slab" :size 14 :weight 'light))

;;(setq fancy-splash-image (concat doom-private-dir "themes/nebula.png"))
(setq fancy-splash-image (concat doom-user-dir "themes/M-x_butterfly.png"))
;;(setq fancy-splash-image (concat doom-private-dir "themes/doom-emacs-color.png"))
(setq doom-theme 'doom-dracula)

(global-visual-line-mode t)
(global-subword-mode 1)
(setq display-line-numbers-type t
      default-directory "~"
      delete-by-moving-to-trash t
      auto-save-default t
      browse-url-browser-function 'eww-browse-url)

(after! undo-fu
  (setq undo-limit        10000000     ;; 1MB
        undo-strong-limit 100000000    ;; 100MB
        undo-outer-limit  1000000000)) ;; 1GB


;; -------------------------------
;; Themes
;; -------------------------------

(use-package! modus-themes
  :defer t
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
  ;;(modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; -------------------------------
;; Evil
;; -------------------------------

(after! evil
  (setq evil-want-fine-undo t))

(use-package! evil-owl
  :defer t
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.5)))
  (set-popup-rule! "^\\*evil-owl\\*" :size 0.5)
  (evil-owl-mode))

(after! eshell
  (add-hook! 'eshell-directory-change-hook
    (company-mode (if (file-remote-p default-directory) -1 +1))))

;; -------------------------------
;; Dired
;; -------------------------------

(defun xah-dired-sort ()
  ;; Sort dired dir listing in different ways.
  ;; URL `http://ergoemacs.org/emacs/dired_sort.html'
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "extension" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
     ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
     ((equal -sort-by "extension") (setq -arg "-Al --si --time-style long-iso -X"))
     ((equal -sort-by "dir") (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other -arg )))

(after! tramp
  (setq tramp-default-method "sshx"
        remote-file-name-inhibit-cache nil
        tramp-encoding-shell "/bin/bash"
        tramp-default-remote-shell "/bin/bash"))

(after! dired
  ;; Ensure dired-omit-mode is not started with dired. It hides some files transparently:
  (remove-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dired-listing-switches "-Al --si --time-style long-iso -t"))

(use-package! dired-subtree
  :defer t
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
;; json mode
;; -------------------------------
(after! (jsonian flycheck)
  (jsonian-enable-flycheck))

;; -------------------------------
;; embark
;; -------------------------------

;; TODO

;; -------------------------------
;; pdf-tools
;; -------------------------------

(use-package! pdf-tools
  :defer t
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
        org-tags-column -80
        org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(defun org-to-clipboard ()
  "Convert the contents of the current buffer or region from Org
   mode to HTML.  Store the result in the clipboard.
   Code from: https://speechcode.com/blog/org-to-clipboard"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region (region-beginning)
                               (region-end)
                               "org2clip")
    (shell-command-on-region (point-min)
                             (point-max)
                             "org2clip")))

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
(use-package! google-translate
  :defer t
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))


;; -------------------------------
;; Edit with emacs Everywhere
;; https://github.com/dmgerman/editWithEmacs.spoon
;; -------------------------------
(load! "../.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon")


;; -------------------------------
;; Mail
;; -------------------------------

(use-package! org-msg
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng d:nil"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-default-alternatives '((new . (utf-8 html))
                                       (reply-to-text . (utf-8 html))
                                       (reply-to-html . (utf-8 html)))
        org-msg-posting-style 'top-posting))

(after! mu4e
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-mu-binary (executable-find "mu")
        mu4e-root-maildir "~/.maildir"
        mu4e-get-mail-command (concat (executable-find "mbsync") " -a")

        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail

        mu4e-update-interval (* 5 60)
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        mu4e-sent-messages-behavior 'delete
        message-kill-buffer-on-exit t

        mu4e-personal-addresses '("bartek@rndity.com")

        mu4e-use-fancy-chars t
        mu4e-headers-visible-lines 10
        mu4e-headers-visible-columns 100
        mu4e-split-view 'vertical
        mu4e-headers-skip-duplicates t
        message-cite-reply-position 'above
        shr-color-visible-luminance-min 80
        shr-color-visible-distance-min 5
        shr-use-colors nil

        mu4e-headers-date-format "%y-%m-%d"
        mu4e-headers-time-format "⧖ %H:%M"
        mu4e-search-results-limit 1000
        mu4e-index-cleanup t)

  (setq mu4e-headers-fields
        '((:account-stripe . 1)
          (:human-date . 12)
          (:flags . 6)
          (:from-or-to . 25)
          (:subject . 50)
          (:maildir)))

  (set-email-account! "bartek@rndity.com"
                      '((user-mail-address . "bartek@rndity.com")
                        (user-full-name . "Bartłomiej Świercz")
                        (mu4e-drafts-folder . "/bartek@rndity.com/[Gmail]/Drafts")
                        (mu4e-sent-folder . "/bartek@rndity.com/[Gmail]/Sent Mail")
                        (mu4e-refile-folder . "/bartek@rndity.com/[Gmail]/All Mail")
                        (mu4e-trash-folder . "/bartek@rndity.com/[Gmail]/Bin")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-user . "bartek@rndity.com")
                        (org-msg-greeting-fmt . "

#+begin_signature
--
*Bartłomiej Świercz*
/Chief Executive Officer of/ *[[https://rndity.com][rndity;]]*

/tel: +48 603 717 633/
#+end_signature"

                                              ))
                      t)

  (setq +mu4e-gmail-accounts '(("bartek@rndity.com"     "/bartek@rndity.com")))

  (setq mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask)

  (setq mu4e-bookmarks
  '(( :name  "Unread messages"
             :query "flag:unread AND NOT flag:trashed and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
             :key ?u)
    ( :name "Today's messages"
            :query "date:today..now and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?t)
    ( :name "Last 7 days"
            :query "date:7d..now and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?w)
    ( :name "Messages with images"
            :query "mime:image/* and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?i)
    ( :name "Messages with PDFs"
            :query "mime:application/pdf and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?p)
    ( :name "Meeting invitations"
            :query "file:/\.ics$/"
            :key ?m)))

  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam")
  )
