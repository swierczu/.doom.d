;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! modus-themes)
(package! vue-mode)
(package! org-super-agenda)
(package! prettier-js)
(package! git-commit)
(package! pocket-reader)
(package! gnugo)
(package! powerline)
(package! dired-narrow)
(package! dired-subtree)
(package! command-log-mode
  :recipe (:host github :repo "lewang/command-log-mode"))
(package! google-translate)
(package! kubernetes)
(package! kubernetes-evil)
(package! nano-theme
  :recipe (:host github :repo "rougier/nano-theme"))
(package! openapi-yaml-mode
  :recipe (:host github :repo "magoyette/openapi-yaml-mode"))
(package! jq-mode
  :recipe (:host github :repo "ljos/jq-mode"))
(package! evil-owl)
(package! evil-goggles)
;; Disable json-mode and use jsonian instead:
(package! json-mode :disable t)
(package! jsonian
  :recipe (:host github :repo "iwahbe/jsonian"))
(package! ct
  :recipe (:host github :repo "neeasade/ct.el"))
(package! languagetool
  :recipe (:host github :repo "PillFall/languagetool.el"))
(package! emacs-powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus"))
(package! eshell-vterm ;:disable t
  :recipe (:host github :repo "iostapyshyn/eshell-vterm"))
(package! peep-dired)
(package! org-roam-ui)
(package! empv)
