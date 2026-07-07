;;; +shell+terminal.el -*- lexical-binding: t; -*-

(use-package! eshell
  :defer t
  :custom
  (eshell-visual-commands nil)
  :config
  (map! :mode eshell-mode
        :i "C-r" #'consult-history
        :i "RET" #'eshell-send-input
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
  (setq-default vterm-shell "/usr/local/bin/fish")
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
  (setq proced-auto-update-interval 1
        proced-auto-update-flag 'visible
        proced-show-remote-processes t
        proced-descent t
        proced-goal-attribute nil
        proced-tree-flag t
        proced-enable-color-flag t
        proced-format 'medium
        proced-filter 'user)

  ;; source: https://rahuljuliato.com/posts/proced-macos
  (when (eq system-type 'darwin)
    (defvar emacs-solo--proced-ps-cache (make-hash-table))
    (defvar emacs-solo--proced-ps-timer nil)

    (defun emacs-solo--proced-ps-do-refresh ()
      (make-process
       :name "proced-ps-refresh"
       :buffer (generate-new-buffer " *proced-ps-temp*")
       :command '("env" "LC_ALL=C" "ps" "-axo"
		  "pid=,%cpu=,%mem=")
       :noquery t
       :sentinel
       (lambda (proc _event)
	 (when (eq (process-status proc) 'exit)
	   (let ((new-cache (make-hash-table)))
	     (with-current-buffer (process-buffer proc)
	       (goto-char (point-min))
	       (while (not (eobp))
		 (when (looking-at
			(rx (* blank)
			    (group (+ digit))
			    (+ blank)
			    (group (+ (any digit ?.)))
			    (+ blank)
			    (group (+ (any digit ?.)))))
		   (puthash
		    (string-to-number (match-string 1))
		    (cons (string-to-number
			   (match-string 2))
			  (string-to-number
			   (match-string 3)))
		    new-cache))
		 (forward-line 1)))
	     (kill-buffer (process-buffer proc))
	     (setq emacs-solo--proced-ps-cache new-cache))))))

    (defun emacs-solo--proced-pcpu (pid)
      (car (gethash pid emacs-solo--proced-ps-cache)))

    (defun emacs-solo--proced-pmem (pid)
      (cdr (gethash pid emacs-solo--proced-ps-cache)))

    (add-hook 'proced-mode-hook
	      (lambda ()
		(setq emacs-solo--proced-ps-timer
		      (run-with-timer 0 2
				      #'emacs-solo--proced-ps-do-refresh))))

    (add-hook 'kill-buffer-hook
	      (lambda ()
		(when (and (derived-mode-p 'proced-mode)
			   (timerp emacs-solo--proced-ps-timer))
		  (cancel-timer emacs-solo--proced-ps-timer)
		  (setq emacs-solo--proced-ps-timer nil))))

    (setq proced-custom-attributes
	  (list
	   (lambda (attrs)
	     (when-let*
		 ((pid (cdr (assq 'pid attrs)))
		  (v (emacs-solo--proced-pcpu pid)))
	       (cons 'pcpu v)))
	   (lambda (attrs)
	     (when-let*
		 ((pid (cdr (assq 'pid attrs)))
		  (v (emacs-solo--proced-pmem pid)))
	       (cons 'pmem v)))))))

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
  (gptel-make-ollama "remote-ollama-qwen3-coder"
    :host "localhost:11435"
    :stream t
    :models '(qwen3-coder:30b))
  (gptel-make-ollama "ollama-phi4-mini"
    :host "localhost:11434"
    :stream t
    :models '(phi4-mini:latest))
  (gptel-make-ollama "ollama-qwen2.5-coder"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder:7b)))

;; Load personal gptel configuration:
(when (string-equal system-type "darwin")
  (let ((file-path (expand-file-name "priv/gptel-key.el" doom-user-dir)))
    (when (file-exists-p file-path)
      (load file-path))))

(use-package! dwim-shell-command
  :config
  ;; from: https://taonaw.com/2026/06/03/correcting-photo-orientation-for-orgmode.html
  (defun other/dwim-image-auto-orient ()
    "Auto-orient images based on EXIF data using mogrify."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Auto-orient images"
     "mogrify -auto-orient '<<f>>'"
     :utils "mogrify"
     :silent-success t)))






