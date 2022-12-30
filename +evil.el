;;; +evil.el -*- lexical-binding: t; -*-

(use-package! evil
  :config
  (setq evil-want-fine-undo t))

(use-package! evil-owl
  :defer t
  :after evil
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.5)))
  (set-popup-rule! "^\\*evil-owl\\*" :size 0.5)
  (evil-owl-mode))

(use-package evil-goggles
  :defer t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))
