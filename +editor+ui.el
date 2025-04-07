;;; +editor+ui.el -*- lexical-binding: t; -*-

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
        :desc "Open with system browser" "D" #'browse-url-default-browser))

(use-package! doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  (setq doom-dracula-brighter-modeline t
        doom-dracula-brighter-comments nil
        doom-dracula-comment-bg nil
        doom-dracula-colorful-headers nil)
  (setq doom-theme 'doom-dracula)
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
