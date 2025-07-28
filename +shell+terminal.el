;;; +shell+terminal.el -*- lexical-binding: t; -*-

(use-package! eshell
  :defer t
  :custom
  (eshell-visual-commands nil)
  :config
  (map! :mode eshell-mode
        :i "C-r" #'consult-history
        (:when (featurep :system 'macos)
          :i "M-RET" #'detached-eshell-send-input)))

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
(use-package! proced
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

(use-package! detached
  :if (string-equal system-type "darwin")
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
  (setq detached-shell-program "/usr/local/bin/zsh")
  (set-popup-rule! "^\\*detached-" :size 0.5)
  (set-popup-rule! "^\\*Detached" :size 0.3))

(use-package! gptel
  :if (string-equal system-type "darwin")
  :config
  (setq gptel-track-media 't)
  (setq gptel-default-mode 'org-mode)
  (gptel-make-ollama "Ollama-deepseek"
    :host "localhost:11434"
    :stream t
    :models '(deepseek-r1:latest))
  (gptel-make-ollama "Ollama-mistral"
    :host "localhost:11434"
    :stream t
    :models '(mistral:latest)))

;; Load personal gptel configuration:
(when (string-equal system-type "darwin")
  (let ((file-path (expand-file-name "priv/gptel-key.el" doom-user-dir)))
    (when (file-exists-p file-path)
      (load file-path))))
