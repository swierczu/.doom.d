;;; +internet+multimedia.el -*- lexical-binding: t; -*-

(use-package! browse-url
  :config
  (setq browse-url-handlers
        '(("^https?://instagram\\.com" . browse-url-default-browser)
          ("^https?://facebook\\.com" . browse-url-default-browser)
          ("^https?://.*youtube\\.com" . browse-url-default-browser)
          ("^https?://.*youtu\\.be" . browse-url-default-browser)
          ("^https?://github\\.com" . browse-url-default-browser)
          ("^https?://gitlab\\.com" . browse-url-default-browser)
          ("^https?://codeberg\\.org" . browse-url-default-browser)
          ("^https?://reddit\\.com" . browse-url-default-browser)
          ("^https?://www\\.reddit\\.com" . browse-url-default-browser)
          ("^https?://figma\\.com" . browse-url-default-browser)
          ("^https?://.*\\.google\\.com" . browse-url-default-browser)
          ("^https?://teams\\.microsoft\\.com" . browse-url-default-browser)
          ("." . eww-browse-url)))
  (when (string-equal system-type "darwin")
    (setq browse-url-generic-program "open")))

(use-package! eww
  :defer t
  :config
  (setq shr-color-visible-luminance-min 80
        shr-color-visible-distance-min 5
        shr-use-colors nil
        shr-use-fonts nil)
  (when (string-equal system-type "darwin")
    (setq eww-retrieve-command
          '("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" "--headless" "--dump-dom" "--disable-gpu" "--disable-dev-shm-usage" "--no-first-run" "--disable-extensions" "--disable-default-apps" "--virtual-time-budget=2000")))

  (defun +my/eww-new ()
    (interactive)
    (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
      (switch-to-buffer (generate-new-buffer "eww"))
      (eww-mode)
      (eww url)))
  
  (defun +other/eww-rename-buffer ()
    "Rename `eww-mode' buffer so sites open in new page."
    ;; http://xahlee.info/emacs/emacs/emacs_eww_web_browser.html
    (let ((title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode)
        (if title
            (rename-buffer (concat "eww " title ) t)
          (rename-buffer "eww" t)))))

  (add-hook 'eww-after-render-hook #'+other/eww-rename-buffer))

(use-package! shrface
  :hook
  (eww-after-render . shrface-mode)
  (nov-mode . shrface-mode)
  (elfeed-show-mode . shrface-mode)
  :config
  (setq shrface-href-versatile t))

(use-package! google-translate
  :defer t
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

;; (use-package! elfeed-org
;;   :after elfeed
;;   :config
;;   (setopt rmh-elfeed-org-files '("~/notes/org/elfeed.org"))
;;   (setopt rmh-elfeed-org-auto-ignore-invalid-feeds t))

;; (with-eval-after-load 'elfeed
;;   (elfeed-org))

(use-package! elfeed
  :defer t
  :init
  (setopt rmh-elfeed-org-files '("~/notes/org/elfeed.org"))
  :config
  (setopt elfeed-search-filter "#50 +unread")
  (defun +my/elfeed-show-xwidget ()
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Sent to xwidget-webkit: %s" link)
        (xwidget-webkit-browse-url link))))

  (defun +my/elfeed-show-browser()
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Sent to default system browser: %s" link)
        (browse-url-default-browser link))))

  (map! :localleader
        :map elfeed-search-mode-map
        :desc "set/unset to read later" "l" (lambda ()
                                              (interactive)
                                              (elfeed-search-toggle-all 'readlater))
        :map elfeed-show-mode-map
        :desc "set to read later" "l" (lambda ()
                                        (interactive)
                                        (elfeed-show-tag 'readlater))
        :map elfeed-show-mode-map
        :desc "open feed in xwidget-webkig" "X" #'+my/elfeed-show-xwidget
        :map elfeed-show-mode-map
        :desc "open feed in browser" "x" #'+my/elfeed-show-browser)
  (when (featurep :system 'macos)
    (setq elfeed-curl-program-name "/usr/local/opt/curl/bin/curl")))

(use-package! timu-imessage
  :defer t
  :commands (timu-imessage timu-imessage-search timu-imessage-doctor)
  :bind (("C-c i i" . timu-imessage)
         ("C-c i s" . timu-imessage-search))
  :config
  (evil-make-overriding-map timu-imessage-chatlist-mode-map)
  (evil-make-overriding-map timu-imessage-convo-mode-map))
