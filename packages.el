;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! vue-mode)
(package! org-super-agenda)
(package! prettier-js)
(package! git-commit)
(package! pocket-reader)
(package! dired-narrow)
(package! dired-subtree)
(package! dired-filter)
(package! command-log-mode
  :recipe (:host github :repo "lewang/command-log-mode"))
(package! google-translate)
(package! kubernetes)
(package! kubernetes-evil)
(package! openapi-yaml-mode
  :recipe (:host github :repo "magoyette/openapi-yaml-mode"))
(package! evil-owl)
(package! evil-goggles)
(package! ct
  :recipe (:host github :repo "neeasade/ct.el"))
(package! languagetool
  :recipe (:host github :repo "PillFall/languagetool.el"))
(package! emacs-powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus"))
(package! org-roam-ui)
(package! empv)
(package! eat ;:disable t
  :recipe (:host codeberg :repo "akib/emacs-eat"))
(package!  mermaid-mode
  :recipe (:host github :repo "abrochard/mermaid-mode"))
(package!  ob-mermaid
  :recipe (:host github :repo "arnm/ob-mermaid"))
(package!  org-noter
  :recipe (:host github :repo "org-noter/org-noter"))
(package!  chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell"))
(package! xr)
(package! detached)
(package! autothemer)
(package! spacious-padding)

;; For testing and checking:
(package! ef-themes
  :recipe (:host sourcehut :repo "protesilaos/ef-themes"))
(package! swagg
  :recipe (:host github :repo "isamert/swagg.el"))
(package! verb)
(package! casual)

;; Unpined packages:
(unpin! (:tools docker)
        ;;(:lang org)
        ;;dirvish
        )
