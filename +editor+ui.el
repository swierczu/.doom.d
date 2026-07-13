;;; +editor+ui.el -*- lexical-binding: t; -*-

(use-package! emacs
  :config
  (setq fancy-splash-image (concat doom-user-dir "themes/M-x_butterfly.png"))
  (setq ns-use-proxy-icon nil)
  (global-visual-line-mode t)

  (when (string-equal system-type "darwin")
    (setq doom-font (font-spec :family "Iosevka Term SS04"
                               :size 15.0 :weight 'light)
          doom-big-font (font-spec :family "Iosevka Term SS04"
                                   :size 20.0 :weight 'light)
          doom-variable-pitch-font (font-spec :family "Iosevka Term Slab"
                                              :size 15.0 :weight 'light)))

  (when (string-equal system-type "android")
    (doom-big-font-mode))

  (add-to-list 'default-frame-alist '(undecorated . nil))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Code from: https://tecosaur.github.io/emacs-config/config.html#window-title
  (setq frame-title-format
        '(""
          (:eval
           (format (if (buffer-modified-p)  " ◯ %s ◯ " " ●  %s ● ") (buffer-name)))
          (:eval
           (format (if (buffer-modified-p)  " ↺ %s ↻ " " ⇛ %s ⇚ ")
                   (if buffer-file-name
                       (file-name-directory (buffer-file-name))
                     (when default-directory
                       (if-let* ((remote (file-remote-p default-directory)))
                           remote
                         default-directory)))))
          (:eval
           (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
             (unless (string= "-" project-name)
               (format " [%s]" project-name)))))))

(use-package! pixel-scroll
  :hook (emacs-startup . pixel-scroll-precision-mode)
  :config
  (setq pixel-scroll-precision-interpolate-page t))

(use-package! subword
  :hook (emacs-startup . global-subword-mode))

(use-package! display-line-numbers
  :config
  (setq display-line-numbers-type nil))

(use-package! undo-tree
  :config
  (setq undo-limit        10000000)   ;; 1MB
  (setq undo-strong-limit 100000000)  ;; 100MB
  (setq undo-outer-limit  1000000000) ;; 1GB
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t))

(use-package! embark
  :config
  (map! :map embark-url-map
        :desc "Open with system browser" "D" #'browse-url-default-browser
        :desc "Open with xWidgets" "X" #'xwidget-webkit-browse-url))

(use-package! doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  (setq doom-dracula-brighter-modeline t
        doom-dracula-brighter-comments nil
        doom-dracula-comment-bg nil
        doom-dracula-colorful-headers nil)
  (when (string-equal system-type "darwin")
    (setq doom-theme 'doom-dracula))
  (when (string-equal system-type "android")
    (setq doom-theme 'dichromacy))
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package! doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package! spacious-padding
  :defer t
  :hook (after-init . spacious-padding-mode)
  :config
  (setq! spacious-padding-widths '(:internal-border-width 8
                                   :header-line-width 2
                                   :mode-line-width 2
                                   :tab-width 4
                                   :right-divider-width 8
                                   :scroll-bar-width 8)))
