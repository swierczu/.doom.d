;;; +lang+web.el -*- lexical-binding: t; -*-

(use-package! dart-mode
  :defer t
  :hook (dart-mode . lsp)
  :config
  (setq flutter-sdk-path "~/sdk/flutter")
  (setq lsp-dart-flutter-sdk flutter-sdk-path)
  (setq lsp-dart-sdk-dir (concat flutter-sdk-path "/bin/cache/dart-sdk")))
