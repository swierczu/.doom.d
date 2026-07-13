;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Globals & defaults
;; -------------------------------
(use-package! emacs
  :config
  ;;(setq shell-file-name (executable-find "bash"))
  (setq shell-file-name "/usr/local/bin/bash")
  ;; Fix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44007
  ;; Taken from: https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu/?utm_source=share&utm_medium=web2x&context=3
  (setq process-adaptive-read-buffering nil)
  (setq read-process-output-max (* 4 1024 1024))

  (setq default-directory "~")
  (setq delete-by-moving-to-trash t)
  (setq auto-save-default t))

;; -------------------------------
;; Evil
;; -------------------------------
(load! "+evil.el")

;; -------------------------------
;; Editor and UI
;; -------------------------------
(load! "+editor+ui.el")

;; -------------------------------
;; Shells and terminals
;; -------------------------------
(load! "+shell+terminal.el")

;; -------------------------------
;; Dired and Tramp
;; -------------------------------
(load! "+dired+tramp.el")

;; -------------------------------
;; Spelling and grammar
;; -------------------------------
(load! "+spelling+grammar.el")

;; -------------------------------
;; Programming languages and modes
;; -------------------------------
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(load! "+lang+go.el")
(load! "+lang+dart.el")
(load! "+lang+web.el")
(load! "+lang+json.el")
(load! "+lang+sql.el")

;; -------------------------------
;; org-mode and other documents
;; -------------------------------
(load! "+org+doc.el")

;; -------------------------------
;; Internet services & multimedia
;; -------------------------------
(load! "+internet+multimedia.el")

;; -------------------------------
;; Edit with emacs Everywhere
;; https://github.com/dmgerman/editWithEmacs.spoon
;; -------------------------------
(if (featurep :system 'macos)
    (load! "../.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon.el"))

;; -------------------------------
;; Mail
;; -------------------------------
(load! "+mail.el")

;; -------------------------------
;; Calendar
;; -------------------------------
(load! "+calendar.el")
