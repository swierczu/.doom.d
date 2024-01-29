;;; +dired+tramp.el -*- lexical-binding: t; -*-

;; Workaround for 'Forbidden reentrant call of Tramp'
;; source: https://github.com/emacs-lsp/lsp-mode/issues/2514#issuecomment-759452037
(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))

(use-package! tramp
  :defer t
  :config
  (setq tramp-default-method "sshx")
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-encoding-shell "/bin/sh")
  (setq tramp-default-remote-shell "/bin/sh")
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; Workaround for 'Forbidden reentrant call of Tramp'
  ;; source: https://github.com/emacs-lsp/lsp-mode/issues/2514#issuecomment-759452037
  (advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around))

(use-package! dired
  :defer t
  :config
  ;; Ensure dired-omit-mode is not started with dired. It hides some files transparently:
  (remove-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dired-listing-switches "-Al --si --time-style long-iso -t"
        dired-mouse-drag-files t
        dired-async-mode 1))

(use-package! dired-subtree
  :defer t
  :after dired
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))

(defun +other/dired-sort ()
  "Sort dired dir listing in different ways. Handy switch."
  ;; Original code from: http://ergoemacs.org/emacs/dired_sort.html
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by (completing-read "Sort by:" '( "date" "size" "name" "extension" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
     ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
     ((equal -sort-by "extension") (setq -arg "-Al --si --time-style long-iso -X"))
     ((equal -sort-by "dir") (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other -arg )))

(use-package! docker
  :config
  ;; workaround for https://github.com/Silex/docker.el/issues/186
  (setq docker-open-hook '(docker-container-update-status-async)))
