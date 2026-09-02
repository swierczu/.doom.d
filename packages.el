;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! vue-mode)
;; (package! pocket-reader)
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
(package! rainbow-delimiters)
(package! nov)
(package! shrface)
(package! fish-mode)
(when (string-equal system-type "darwin")
  (package! detached)
  (package! elfeed-tube-mpv)
  (package! nov-xwidget
    :recipe (:host github :repo "chenyanming/nov-xwidget"))
  (package! mpv)
  (package! dwim-shell-command
    :recipe (:host github :repo "xenodium/dwim-shell-command"))
  (package!  treesitter-context
    :recipe (:host github :repo "zbelial/treesitter-context.el"))
  (package! pg
    :recipe (:host github :repo "emarsden/pg-el"))
  (package! pgmacs
    :recipe (:host github :repo "emarsden/pgmacs"))
  (package! tramp-rpc
    :recipe (:host github :repo "ArthurHeymans/emacs-tramp-rpc"))
  (package! msgpack))

;; For testing and checking:
(package! swagg
  :recipe (:host github :repo "isamert/swagg.el"))
(package! verb)
(package! dslide)
(package! powerthesaurus)
(package! go-fill-struct
  :recipe (:host github :repo "s-kostyaev/go-fill-struct"))
(package! go-add-tags
  :recipe (:host github :repo "emacsorphanage/go-add-tags"))
(package! go-prettify-mode
  :recipe (:host codeberg :repo "snyssfx/go-prettify-mode.el"))
(package! go-tag)
(package! go-template-mode)
(package! jinx)
(package! emacs-imessage
  :recipe (:host gitlab :repo "aimebertrand/emacs-imessage"))
;; (package!
;;   :recipe (:host github :repo ""))

;; Unpined packages:
(unpin! (:app rss))
