;;; +tools.el -*- lexical-binding: t; -*-

(use-package magit
  :custom
  (when (string-equal system-type "darwin")
    (magit-git-executable "/usr/local/bin/git")))
