;;; +tools.el -*- lexical-binding: t; -*-

(use-package magit
  :config
  (setq magit-process-connection-type nil)
  (when (string-equal system-type "darwin")
    (setq magit-git-executable "/usr/local/bin/git")))
