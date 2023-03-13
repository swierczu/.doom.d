;;; +shell+terminal.el -*- lexical-binding: t; -*-

(use-package! eshell
  :defer t
  :after eat
  :config
  (add-hook! 'eshell-directory-change-hook
    (company-mode (if (file-remote-p default-directory) -1 +1)))
  (add-hook! 'eshell-load-hook #'eat-eshell-mode)
  (add-hook! 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (map! :mode eshell-mode
        :ni "C-r" #'consult-history)

  ;; temporal fix for regression in emacs 28:
  ;; restore previous version of eshell--complete-commands-list function
  ;; https://github.com/company-mode/company-mode/issues/1322
  (defun eshell-fix-1322 ()
    "Generate list of applicable, visible commands."
    (let ((filename (pcomplete-arg)) glob-name)
      (if (file-name-directory filename)
          (if eshell-force-execution
              (pcomplete-dirs-or-entries nil #'file-readable-p)
            (pcomplete-executables))
        (if (and (> (length filename) 0)
                 (eq (aref filename 0) eshell-explicit-command-char))
            (setq filename (substring filename 1)
                  pcomplete-stub filename
                  glob-name t))
        (let* ((paths (eshell-get-path))
               (cwd (file-name-as-directory
                     (expand-file-name default-directory)))
               (path "") (comps-in-path ())
               (file "") (filepath "") (completions ()))
          ;; Go thru each path in the search path, finding completions.
          (while paths
            (setq path (file-name-as-directory
                        (expand-file-name (or (car paths) ".")))
                  comps-in-path
                  (and (file-accessible-directory-p path)
                       (file-name-all-completions filename path)))
            ;; Go thru each completion found, to see whether it should
            ;; be used.
            (while comps-in-path
              (setq file (car comps-in-path)
                    filepath (concat path file))
              (if (and (not (member file completions)) ;
                       (or (string-equal path cwd)
                           (not (file-directory-p filepath)))
                       (if eshell-force-execution
                           (file-readable-p filepath)
                         (file-executable-p filepath)))
                  (setq completions (cons file completions)))
              (setq comps-in-path (cdr comps-in-path)))
            (setq paths (cdr paths)))
          ;; Add aliases which are currently visible, and Lisp functions.
          (pcomplete-uniquify-list
           (if glob-name
               completions
             (setq completions
                   (append (if (fboundp 'eshell-alias-completions)
                               (eshell-alias-completions filename))
                           (eshell-winnow-list
                            (mapcar
                             (lambda (name)
                               (substring name 7))
                             (all-completions (concat "eshell/" filename)
                                              obarray #'functionp))
                            nil '(eshell-find-alias-function))
                           completions))
             (append (and (or eshell-show-lisp-completions
                              (and eshell-show-lisp-alternatives
                                   (null completions)))
                          (all-completions filename obarray #'functionp))
                     completions)))))))

  (advice-add 'eshell--complete-commands-list :override #'eshell-fix-1322))

(use-package! eat
  :defer t
  )

(use-package! vterm
  :config
  (add-to-list 'vterm-tramp-shells '("sshx" "/bin/sh"))
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh")))

;; Proced config
;; source: https://laurencewarne.github.io/emacs/programming/2022/12/26/exploring-proced.html
(use-package proced
  :commands proced
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 1
        proced-goal-attribute nil
        ;; only for Emacs 29:
        proced-enable-color-flag t)
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm)))
  (setq-default proced-format 'custom))
