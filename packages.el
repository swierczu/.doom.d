;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! vue-mode)
(package! pocket-reader)
(package! dired-narrow)
(package! dired-subtree)
(package! dired-filter)
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
(package! empv)
(package! eat
  :recipe (:host codeberg :repo "akib/emacs-eat"))
(package!  mermaid-mode
  :recipe (:host github :repo "abrochard/mermaid-mode"))
(package!  ob-mermaid
  :recipe (:host github :repo "arnm/ob-mermaid"))
(package!  org-noter
  :recipe (:host github :repo "org-noter/org-noter"))
(package! xr)
(package! detached)
(package! autothemer)
(package! spacious-padding)
(package! elfeed-tube)
(package! elfeed-tube-mpv)

;; For testing and checking:
(package! swagg
  :recipe (:host github :repo "isamert/swagg.el"))
(package! verb)
(package! dslide)
(package! gptel)

;; Unpined packages:
(unpin! (:tools docker)
        ;;(:lang org)
        ;;dirvish
        )
