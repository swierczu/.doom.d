;;; +internet+multimedia.el -*- lexical-binding: t; -*-

(use-package! eww
  :defer t
  :config
  (setq shr-color-visible-luminance-min 80
        shr-color-visible-distance-min 5
        shr-use-colors nil
        shr-use-fonts nil)
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
  :defer t
  :config
  (setq empv-invidious-instance "https://inv.bp.projectsegfau.lt/api/v1")
  (setq empv-mpv-args nil)
  (add-to-list 'empv-mpv-args "--save-position-on-quit")
  (add-to-list 'empv-mpv-args "--ytdl-format=best")
  (add-to-list 'empv-mpv-args "--vid=auto"))

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
                                        (elfeed-show-tag 'readlater))))

(use-package! chatgpt-shell
  :defer t
  :commands
  (chatgpt-shell dall-e-shell)
  :init
  (setq chatgpt-shell-openai-key
        (lambda ()
          (nth 0 (process-lines "pass" "show" "OpenAI/bartek@rndity.com")))))
