;;; +internet+multimedia.el -*- lexical-binding: t; -*-

(use-package! eww
  :defer t
  :config
  (setq shr-color-visible-luminance-min 80
        shr-color-visible-distance-min 5
        shr-use-colors nil
        shr-use-fonts nil)
  (when (string-equal system-type "darwin")
    (setq eww-retrieve-command
          '("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" "--headless" "--dump-dom" "--disable-gpu" "--virtual-time-budget=2000")))
  (add-hook 'eww-after-render-hook #'+other/eww-rename-buffer))

(defun +other/eww-rename-buffer ()
  "Rename `eww-mode' buffer so sites open in new page."
  ;; http://xahlee.info/emacs/emacs/emacs_eww_web_browser.html
  (let ((title (plist-get eww-data :title)))
    (when (eq major-mode 'eww-mode)
      (if title
          (rename-buffer (concat "eww " title ) t)
        (rename-buffer "eww" t)))))

(use-package! google-translate
  :defer t
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package! empv
  :if (string-equal system-type "darwin")
  :defer t
  :config
  (setq empv-invidious-instance "https://inv.bp.projectsegfau.lt/api/v1")
  (setq empv-mpv-args nil)
  (add-to-list 'empv-mpv-args "--save-position-on-quit")
  (add-to-list 'empv-mpv-args "--ytdl-format=best")
  (add-to-list 'empv-mpv-args "--vid=auto"))

(defun +my/elfeed-show-xwidget ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Sent to xwidget-webkit: %s" link)
      (xwidget-webkit-browse-url link))))

(use-package! elfeed
  :defer t
  :config
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
        :desc "open feed in xwidget-webkig" "x" #'+my/elfeed-show-xwidget)
  (when (featurep :system 'macos)
    (setq elfeed-curl-program-name "/usr/local/opt/curl/bin/curl")))

(use-package! elfeed-tube
  :if (string-equal system-type "darwin")
  :defer t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package! elfeed-tube-mpv
  :if (string-equal system-type "darwin")
  :defer t
  :after elfeed
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))
