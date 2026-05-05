;;; +lang+go.el -*- lexical-binding: t; -*-

;; (use-package! go-ts-mode
;;   :hook
;;   (go-ts-mode . go-prettify-mode))

(use-package! go-fill-struct
  :defer t
  :after (go-ts-mode)
  :config
  )

(use-package! go-add-tags
  :defer t
  :after (go-ts-mode)
  :config
  )

;; (use-package! go-prettify-mode
;;   :defer t
;;   :after (go-ts-mode))

(use-package go-template-mode
  :after (go-ts-mode)
  :mode (("\\.gotmpl\\'" . go-template-mode)
         ("\\.tpl\\'" . go-template-mode)
         ("\\.tmpl\\'" . go-template-mode)))
