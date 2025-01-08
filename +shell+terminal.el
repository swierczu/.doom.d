;;; +shell+terminal.el -*- lexical-binding: t; -*-

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

(defun eshell-custom-hook ()
  (interactive)
  (map! :mode eshell-mode
        :i "C-r" #'consult-history
        :i "M-RET" #'detached-eshell-send-input)
  (advice-add 'eshell--complete-commands-list :override #'eshell-fix-1322))

(use-package! eshell
  :defer t
  :hook (eshell-directory-change . (lambda ()
                                     (company-mode (if (file-remote-p default-directory) -1 +1))))
  :custom
  (eshell-visual-commands nil)
  :config
  (map! :mode eshell-mode
        :i "C-r" #'consult-history
        :i "M-RET" #'detached-eshell-send-input)
  (advice-add 'eshell--complete-commands-list :override #'eshell-fix-1322))

(use-package! eat
  ;; Don't forget to run tic -x eat.ti (found on the source)
  ;; or run https://elpa.nongnu.org/nongnu-devel/doc/eat.html#Not-Recognized
  :config
  (eat-eshell-mode t))

(use-package! vterm
  :defer t
  :config
  (add-to-list 'vterm-tramp-shells '("sshx" "/bin/sh"))
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh")))

;; Proced config
;; source: https://laurencewarne.github.io/emacs/programming/2022/12/26/exploring-proced.html
(use-package proced
  :commands proced
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid tree pcpu pmem vsize rss start time state (args comm)))
  (setq proced-auto-update-interval 2
        proced-auto-update-flag nil
        proced-show-remote-processes t
        proced-goal-attribute nil
        proced-tree-flag t
        proced-enable-color-flag t
        proced-format 'custom))

(use-package detached
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type))
  :config
  (set-popup-rule! "^\\*detached-" :size 0.5)
  (set-popup-rule! "^\\*Detached" :size 0.3))

(let ((file-path (expand-file-name "priv/gptel-key.el" user-emacs-directory)))
  (when (file-exists-p file-path)
    (load file-path)))

