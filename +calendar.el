;;; +calendar.el -*- lexical-binding: t; -*-

(use-package! calendar
  :config
  (setq calendar-week-start-day 1)
  ;; adding week number to calendar
  ;; taken from https://stackoverflow.com/a/21367291
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-warning-face))
  (setq calendar-intermonth-header
        (propertize "Wk" 'font-lock-face 'font-lock-keyword-face))

  (map! :g "C-c c" #'calendar))
