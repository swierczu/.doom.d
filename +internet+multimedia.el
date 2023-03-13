;;; +internet+multimedia.el -*- lexical-binding: t; -*-

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
  (add-to-list 'empv-mpv-args "--save-position-on-quit")
  )

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
