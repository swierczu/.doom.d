;;; +tools.el -*- lexical-binding: t; -*-

(use-package casual-calc
  :bind (:map calc-mode-map ("?" . #'casual-calc-tmenu)))

(use-package casual-isearch
  :bind (:map isearch-mode-map ("?" . #'casual-isearch-tmenu)))

(use-package casual-dired
  :bind (:map dired-mode-map ("?" . #'casual-dired-tmenu)))
