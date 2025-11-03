;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! vue-mode)
(package! pocket-reader)
(package! dired-narrow)
(package! dired-subtree)
(package! dired-filter)
(package! mu4e-alert)
(package! google-translate)
(package! kubernetes)
(package! kubernetes-evil)
(package! openapi-yaml-mode
  :recipe (:host github :repo "magoyette/openapi-yaml-mode"))
(package! evil-owl)
(package! evil-goggles)
(package! ct
  :recipe (:host github :repo "neeasade/ct.el"))
(package! org-roam-ui)
(package! eat
  :recipe (:host codeberg :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
(package!  mermaid-mode
  :recipe (:host github :repo "abrochard/mermaid-mode"))
(package!  ob-mermaid
  :recipe (:host github :repo "arnm/ob-mermaid"))
(package!  org-noter
  :recipe (:host github :repo "org-noter/org-noter"))
(package! xr)
(package! gnugo)
(package! autothemer)
(package! spacious-padding)
(package! hcl-mode)
(package! hurl-mode
  :recipe (:host github :repo "jaszhe/hurl-mode" :files ("*.el")))
(when (string-equal system-type "darwin")
  (package! detached)
  (package! elfeed-tube)
  (package! elfeed-tube-mpv)
  (package! empv)
  (package! gptel :recipe (:nonrecursive t))
  (package!  treesitter-context
    :recipe (:host github :repo "zbelial/treesitter-context.el"))
  (package! pg
    :recipe (:host github :repo "emarsden/pg-el"))
  (package! pgmacs
    :recipe (:host github :repo "emarsden/pgmacs")))

;; For testing and checking:
(package! swagg
  :recipe (:host github :repo "isamert/swagg.el"))
(package! verb)
(package! dslide)
(package! powerthesaurus)
(package! auto-dim-other-buffers)
;; (package!
;;   :recipe (:host github :repo ""))

;; Unpined packages:
(unpin! (:app rss))
(unpin! lsp-dart)
