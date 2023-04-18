;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Globals & defaults
;; -------------------------------
(use-package! emacs
  :config
  (setq user-full-name "Bartłomiej Świercz")
  (setq user-mail-address "bartek@rndity.com")
  (setq doom-font (font-spec :family "Iosevka Term SS04" :size 15.0 :weight 'light))
  (setq doom-big-font (font-spec :family "Iosevka Term SS04" :size 20.0 :weight 'light))
  (setq doom-variable-pitch-font (font-spec :family "Iosevka Term Slab" :size 15.0 :weight 'light))
  (setq fancy-splash-image (concat doom-user-dir "themes/M-x_butterfly.png"))
  (setq doom-theme 'doom-dracula)
  (global-visual-line-mode t)
  (global-subword-mode 1)
  (setq calendar-week-start-day 1)
  (setq display-line-numbers-type nil)
  (setq default-directory "~")
  (setq delete-by-moving-to-trash t)
  (setq auto-save-default t)
  (setq browse-url-handlers
        '(("^https?://instagram\\.com" . browse-url-default-browser)
          ("^https?://facebook\\.com" . browse-url-default-browser)
          ("^https?://.*youtube\\.com" . browse-url-default-browser)
          ("^https?://.*youtu\\.be" . browse-url-default-browser)
          ("." . eww-browse-url)))
  )

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

(load! "+lang+web.el")
(load! "+lang+json.el")

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

(load! "../.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon.el")

;; -------------------------------
;; Mail
;; -------------------------------

(load! "+mail.el")
