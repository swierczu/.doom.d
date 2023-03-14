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