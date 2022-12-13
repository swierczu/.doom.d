;;; +shell+terminal.el -*- lexical-binding: t; -*-

(use-package! eshell
  :defer t
  :config
  (add-hook! 'eshell-directory-change-hook
    (company-mode (if (file-remote-p default-directory) -1 +1)))
  (map! :mode eshell-mode
        :ni "C-r" #'consult-history))

(use-package eshell-vterm
  :defer t
  :after eshell
  :config
  (eshell-vterm-mode))

(use-package! vterm
  :config
  (add-to-list 'vterm-tramp-shells '("sshx" "/bin/sh"))
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh")))
