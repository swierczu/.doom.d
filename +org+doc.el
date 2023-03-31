;;; +org+doc.el -*- lexical-binding: t; -*-

(use-package! org
  :defer t
  :init
  (setq org-directory "~/org")
  :config
  (setq org-attach-id-dir "~/Dropbox/notes/.attach/")
  (setq org-attach-directory "~/Dropbox/notes/.attach/")
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-tags-column -80)
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-show-all-today t)
  (set-face-attribute 'org-document-title nil :height 1.4 :weight 'extra-bold)
  (set-face-attribute 'org-level-1 nil :extend nil :height 1.3)
  (set-face-attribute 'org-level-2 nil :extend nil :height 1.2)
  (set-face-attribute 'org-level-3 nil :extend nil :height 1.1)
  (set-face-attribute 'org-tag nil
                      :weight 'light
                      :height 1.0
                      :box t
                      :foreground "#bd93f9"
                      :inverse-video t)
  (setq org-tag-faces
        '(("noexport" . (:foreground "#b6b6b2" :height 1.0))
          ("project"  . (:foreground "#ff79c6"))
          ("meeting"  . (:foreground "#ff79c6")))))

;; org-protocol:
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  ;; Code from: https://github.com/sprig/org-capture-extension/blob/master/README.md
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

(defun +other/org-to-clipboard ()
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

(defun +other/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  ;; Code from https://github.com/SqrtMinusOne/dotfiles/blob/4b176a5bb1a5e20a7fdd7398b74df79701267a7e/.emacs.d/init.el
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(use-package! pdf-tools
  :defer t
  :magic
  ("%PDF" . pdf-view-mode)
  :init
  (pdf-tools-install :no-query)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)
  (add-hook! 'pdf-view-mode-hook
    (pdf-view-midnight-minor-mode t)))

(use-package! deft
  :defer t
  :after org
  :config
  (setq deft-directory org-directory
        deft-extensions '("org" "txt" "md" "tex")
        deft-use-filename-as-title t
        deft-recursive t))

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
        (format "%s ${doom-hierarchy:*} %s"
                (propertize "${type:10}" 'face 'font-lock-keyword-face)
                (propertize "${doom-tags:42}" 'face 'org-tag)))
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)))

(use-package! org-roam-capture
  :after org-roam
  :config
  (setq org-roam-capture-templates nil)
  ;; capture default note
  (add-to-list 'org-roam-capture-templates
               '("d" "📝 Default note" plain "%?"
                 :if-new
                 (file+head "default/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  ;; capture notes releated to projects
  (add-to-list 'org-roam-capture-templates
               '("p" "🗒 Projects")
               t)
  (add-to-list 'org-roam-capture-templates
               '("pn" "🗒 New project note" entry "* %?"
                 :if-new
                 (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :project:\n\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  (add-to-list 'org-roam-capture-templates
               '("pt" "🏷 New project node/tag" plain "%?"
                 :if-new
                 (file+head "projects/project_${slug}.org"
                            ":PROPERTIES:\n\
:ROAM_ALIASES: ${title}\n\
:END:\n\
#+filetags: :project:\n\
#+title: ${title} Project\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  ;; capture notes releated to rnd.guru
  (add-to-list 'org-roam-capture-templates
               '("r" "🦙 rnd.guru")
               t)
  (add-to-list 'org-roam-capture-templates
               '("rn" "🗒 New note" entry "* %?"
                 :if-new
                 (file+head "rnd.guru/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :rnd.guru:\n\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  (add-to-list 'org-roam-capture-templates
               '("rt" "🏷 New node/tag" plain "%?"
                 :if-new
                 (file+head "rnd.guru/company_${slug}.org"
                            ":PROPERTIES:\n\
:ROAM_ALIASES: ${title}\n\
:END:\n\
#+filetags: :rnd.guru:\n\
#+title: ${title} Company\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  ;; capture notes releated to technology
  (add-to-list 'org-roam-capture-templates
               '("t" "💾 Technology")
               t)
  (add-to-list 'org-roam-capture-templates
               '("tn" "🗒 New technology note" entry "* %?"
                 :if-new
                 (file+head "technology/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :technology:\n\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  (add-to-list 'org-roam-capture-templates
               '("tt" "🏷 New technology node/tag" plain "%?"
                 :if-new
                 (file+head "technology/technology_${slug}.org"
                            ":PROPERTIES:\n\
:ROAM_ALIASES: ${title}\n\
:END:\n\
#+filetags: :technology:\n\
#+title: ${title} Technology\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  ;; capture notes releated to people
  (add-to-list 'org-roam-capture-templates
               '("c" "👨 People / Contacts")
               t)
  (add-to-list 'org-roam-capture-templates
               '("cp" "🏷 New people/contact" entry "* Contact\n** Mail %?\n** Phone \n\n* Projects\n\n* Notes\n"
                 :if-new
                 (file+head "people/contact_${slug}.org"
                            ":PROPERTIES:\n\
:ROAM_ALIASES: ${slug}\n\
:END:\n\
#+filetags: :people:\n\
#+title: ${title}\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  ;; capture notes releated to sport, health and activity
  (add-to-list 'org-roam-capture-templates
               '("s" "🏃 Sport, health & activity")
               t)
  (add-to-list 'org-roam-capture-templates
               '("sn" "🗒 New sport note" entry "* %?"
                 :if-new
                 (file+head "sport/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :sport:\n\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)
  (add-to-list 'org-roam-capture-templates
               '("st" "🏷 New sport node/tag" plain "%?"
                 :if-new
                 (file+head "sport/sport_${slug}.org"
                            ":PROPERTIES:\n\
:END:\n\
#+filetags: :sport:\n\
#+title: ${title}\n")
                 :unnarrowed t
                 :empty-lines 1)
               t)

  )

(use-package! org-roam-dailies
  :after org-roam
  :config
  (setq org-roam-dailies-capture-templates nil)
  (add-to-list 'org-roam-dailies-capture-templates
               '("d" "📋 Daily schedule" entry "* TODO %?\nSCHEDULED: %t"
                 :target
                 (file+head+olp "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"
                                ("Daily schedule"))
                 :unnarrowed t
                 :empty-lines 1)
               t)
  (add-to-list 'org-roam-dailies-capture-templates
               '("n" "📝 Notes" entry "* %<%H:%M> %?"
                 :target
                 (file+head+olp "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"
                                ("Notes"))
                 :unnarrowed t
                 :empty-lines 1)
               t))

(defun +my/hostname(url)
  "Get hostname from given URL"
  (url-host (url-generic-parse-url url)))

(defun +my/web-ref-filename(ref)
  "Generate filename to store bookmark in my org-roam system"
  (concat "web/"
          (+my/hostname ref)
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
                  "%(+my/web-ref-filename \"${ref}\")"
                  "#+TITLE: %(+my/hostname \"${ref}\")\n#+filetags: :bookmark:\n\n"
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
                  "%(+my/web-ref-filename \"${ref}\")"
                  "#+TITLE: %(+my/hostname \"${ref}\")\n#+filetags: :bookmark:\n\n"
                  ("${title}"))
                 :unnarrowed t
                 :empty-lines 1)))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
