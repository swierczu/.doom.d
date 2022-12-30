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
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("c" "Currently clocked-in" item (clock)
                 "Note taken on %U \\\ \n%?"
                 :prepend t)))

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

(use-package! deft
  :defer t
  :after org
  :config
  (setq deft-directory org-directory)
  (setq deft-extensions '("org" "txt" "md" "tex"))
  (setq deft-recursive t))

(use-package! org-roam
  :defer t
  :after org
  :config
  (setq org-roam-directory org-directory)
  (setq org-roam-db-location (concat user-emacs-directory "org-roam.db"))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  ;; code from https://jethrokuan.github.io/org-roam-guide/
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
  (setq org-roam-node-display-template
        (format "${doom-hierarchy:*} %s %s"
                (propertize "${type:12}" 'face 'font-lock-keyword-face)
                (propertize "${doom-tags:42}" 'face 'org-tag)))
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)))

(use-package! org-roam-dailies
  :after org-roam
  :config
  (setq org-roam-dailies-capture-templates nil)
  (add-to-list 'org-roam-dailies-capture-templates
               '("d" "Daily schedule" entry "* TODO %?\nSCHEDULED: %t"
                 :target
                 (file+head+olp "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>\n"
                                ("Daily schedule"))
               :unnarrowed t
               :empty-lines 1))
  (add-to-list 'org-roam-dailies-capture-templates
               '("n" "Notes" entry "* %<%H:%M> %?"
                 :target
                 (file+head+olp "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>\n"
                                ("Notes"))
               :unnarrowed t
               :empty-lines 1)))

(defun +bs/hostname(url)
  (url-host (url-generic-parse-url url)))

(defun +bs/web-ref-filename(ref)
  (concat "web/"
          (+bs/hostname ref)
          ".org"))

(use-package! org-roam-protocol
  :after (org-roam org-protocol)
  :config
  (setq org-roam-capture-ref-templates nil)
  (add-to-list 'org-roam-capture-ref-templates
        '("r" "Web bookmark reference" entry
          "* %?\n \
:PROPERTIES:\n \
:Link: ${ref}\n \
:When: %U\n \
:END:\n"
           :if-new
           (file+head+olp
            "%(+bs/web-ref-filename \"${ref}\")"
            "#+TITLE: %(+bs/hostname \"${ref}\")\n#+filetags: :bookmark:\n\n"
            ("${title}"))
           :unnarrowed t
           :empty-lines 1))
  (add-to-list 'org-roam-capture-ref-templates
        '("b" "Web bookmark citation" entry
          "* %?\n \
:PROPERTIES:\n \
:Link: ${ref}\n \
:When: %U\n \
:END:\n \
#+BEGIN_QUOTE\n \
${body}\n \
#+END_QUOTE\n"
           :if-new
           (file+head+olp
            "%(+bs/web-ref-filename \"${ref}\")"
            "#+TITLE: %(+bs/hostname \"${ref}\")\n#+filetags: :bookmark:\n\n"
            ("${title}"))
           :unnarrowed t
           :empty-lines 1)))
