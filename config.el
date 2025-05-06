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
  (add-to-list 'default-frame-alist '(undecorated . nil))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon nil)
  ;; Fix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44007
  ;; Taken from: https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu/?utm_source=share&utm_medium=web2x&context=3
  (setq process-adaptive-read-buffering nil)
  (setq read-process-output-max (* 4 1024 1024))
  ;; Code from: https://tecosaur.github.io/emacs-config/config.html#window-title
  (setq frame-title-format
        '(""
          (:eval
           (if (string-match-p (regexp-quote (or (bound-and-true-p org-roam-directory) "\u0000"))
                               (or buffer-file-name ""))
               (replace-regexp-in-string
                ".*/[0-9]*-?" "☰ "
                (subst-char-in-string ?_ ?\s buffer-file-name))
             "%b"))
          (:eval
           (if buffer-file-name
               (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s")
                       (file-name-directory (buffer-file-name)))
             (when default-directory
               (when-let* ((remote (file-remote-p default-directory)))
                 (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") remote)))))
          (:eval
           (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
             (unless (string= "-" project-name)
               (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))
          ))
  (global-visual-line-mode t)
  (pixel-scroll-precision-mode t)
  (setq pixel-scroll-precision-interpolate-page t)
  (global-subword-mode 1)

  (setq calendar-week-start-day 1)
  ;; adding week number to calendar
  ;; taken from https://stackoverflow.com/a/21367291
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-warning-face))
  (setq calendar-intermonth-header
        (propertize "Wk" 'font-lock-face 'font-lock-keyword-face))

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
  (setq browse-url-generic-program "open"))

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
