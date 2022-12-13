;;; +org+doc.el -*- lexical-binding: t; -*-

(use-package! org
  :defer t
  :init
  (setq org-directory "~/org")
  :config
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-tags-column -80)
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package! org-roam
  :defer t
  :after org
  :config
  (setq org-roam-directory org-directory)
  (setq org-roam-db-location (concat user-emacs-directory "org-roam.db")))

(use-package! deft
  :defer t
  :after org
  :config
  (setq deft-directory org-directory)
  (setq deft-extensions '("org" "txt" "md" "tex"))
  (setq deft-recursive t))

;; org-protocol:
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

(use-package! org-capture
  :defer t
  :after org
  :config
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

(defun org-to-clipboard ()
  "Convert the contents of the current buffer or region from Org
   mode to HTML.  Store the result in the clipboard."
   ;; Code from: https://speechcode.com/blog/org-to-clipboard"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region (region-beginning)
                               (region-end)
                               "org2clip")
    (shell-command-on-region (point-min)
                             (point-max)
                             "org2clip")))

(use-package! pdf-tools
  :defer t
  :init
  (pdf-tools-install)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t))
