;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Globals & defaults
;; -------------------------------
(use-package! emacs
  :config
  (setq user-full-name "Bartłomiej Świercz")
  (setq user-mail-address "bartek@rndity.com")
  (setq doom-font (font-spec :family "Iosevka Term SS04" :size 14 :weight 'light))
  (setq doom-big-font (font-spec :family "Iosevka Term SS04" :size 20 :weight 'light))
  (setq doom-variable-pitch-font (font-spec :family "Iosevka Term Slab" :size 14 :weight 'light))
  (setq fancy-splash-image (concat doom-user-dir "themes/M-x_butterfly.png"))
  (setq doom-theme 'doom-dracula)
  (global-visual-line-mode t)
  (global-subword-mode 1)
  (setq display-line-numbers-type nil)
  (setq default-directory "~")
  (setq delete-by-moving-to-trash t)
  (setq auto-save-default t)
  (setq browse-url-browser-function 'eww-browse-url))

(use-package! undo-tree
  :config
  (setq undo-limit        10000000)   ;; 1MB
  (setq undo-strong-limit 100000000)  ;; 100MB
  (setq undo-outer-limit  1000000000) ;; 1GB
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t))

(defun +other/file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs. \
   It is usefull when Emacs is slowing down due to large number of open files by Tramp"
  ;; Code from: https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; -------------------------------
;; Evil
;; -------------------------------

(load! "+evil.el")

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

(load! "+lang+web.el")
(load! "+lang+json.el")

;; -------------------------------
;; org-mode and other documents
;; -------------------------------

(load! "+org+doc.el")

;; -------------------------------
;; Google services
;; -------------------------------

(load! "+google.el")

;; -------------------------------
;; Edit with emacs Everywhere
;; https://github.com/dmgerman/editWithEmacs.spoon
;; -------------------------------

(load! "../.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon")

;; -------------------------------
;; Mail
;; -------------------------------

(load! "+mail.el")
