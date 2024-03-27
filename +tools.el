;;; +tools.el -*- lexical-binding: t; -*-

(use-package! casual
  :defer t
  :after calc
  :bind (:map calc-mode-map ("C-o" . 'casual-main-menu)))
