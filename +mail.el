;;; +mail.el -*- lexical-binding: t; -*-

(use-package! mu4e
  :defer t
  :config
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-mu-binary (executable-find "mu")
        mu4e-root-maildir "~/.maildir"
        mu4e-get-mail-command (concat (executable-find "mbsync") " -a")

        ;; Rember to set VAR in msmtpq script:
        ;; EMAIL_CONN_TEST='n'
        ;; EMAIL_QUEUE_QUIET=t
        sendmail-program (executable-find "msmtpq")
        send-mail-function #'smtpmail-send-it
        message-send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header

        mu4e-update-interval (* 5 60)
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        ;; mu4e-sent-messages-behavior 'delete
        mu4e-sent-messages-behavior 'sent
        message-kill-buffer-on-exit t

        mu4e-use-fancy-chars t
        mu4e-headers-visible-lines 10
        mu4e-headers-visible-columns 100
        mu4e-split-view 'vertical
        mu4e-search-skip-duplicates nil
        message-cite-reply-position 'above
        shr-color-visible-luminance-min 80
        shr-color-visible-distance-min 5
        shr-use-colors nil
        shr-use-fonts nil

        mu4e-headers-date-format "%y-%m-%d"
        mu4e-headers-time-format "⧖ %H:%M"
        mu4e-search-results-limit 1000
        mu4e-index-cleanup t)

  (setq mu4e-headers-fields
        '((:account-stripe . 1)
          (:human-date . 12)
          (:flags . 6)
          (:from-or-to . 25)
          (:subject . 50)
          (:maildir)))

  (setq mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask)

  (setq mu4e-maildir-shortcuts nil)

  (set-face-attribute 'gnus-header-subject nil
                      :height 1.4
                      :weight 'extra-bold
                      :foreground "#ffb86c")
  (set-face-attribute 'gnus-header-from nil
                      :height 1.0
                      :weight 'bold
                      :foreground "#ff79c6")

  (add-to-list 'display-buffer-alist
               `(,(regexp-quote mu4e-view-buffer-name)
                 display-buffer-in-side-window
                 (side . right)
                 (window-width . 0.5)))

  (map! :map mu4e-view-mode-map
        :n "q" #'mu4e-view-quit
        :map mu4e-headers-mode-map
        :n "T" #'mu4e-view-mark-thread))

;; Load personal mu4e configuration:
(let ((file-path (expand-file-name "priv/mu4e-personal.el" doom-user-dir)))
  (when (file-exists-p file-path)
    (load file-path)))

(use-package! mu4e-alert
  :after mu4e
  :defer t
  :config
  (setq mu4e-alert-set-default-style 'notifier)
  (setq mu4e-alert-email-notification-types '(count))
  (setq mu4e-alert-interesting-mail-query "flag:unread and date:3d..now AND NOT flag:trashed")
  (mu4e-alert-enable-notifications))

(use-package! mu4e-thread
  :after mu4e
  :defer t
  :config
  (map! :map mu4e-headers-mode-map
        :n "za" #'mu4e-thread-fold-toggle
        :n "zc" #'mu4e-thread-fold
        :n "zo" #'mu4e-thread-unfold
        :n "zm" #'mu4e-thread-fold-all
        :n "zr" #'mu4e-thread-unfold-all))

(use-package! org-msg
  :defer t
  :after (mu4e org)
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:3 ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng d:nil tags:nil")
  (setq org-msg-startup "hidestars indent inlineimages")
  (setq org-msg-default-alternatives '((new . (utf-8 html))
                                       (reply-to-text . (utf-8 html))
                                       (reply-to-html . (utf-8 html))))
  (setq org-msg-posting-style 'top-posting)
  (setq +org-msg-accent-color "#d6001c")
  (setq org-msg-enforce-css
        (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
        \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
               (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
               (font-size '(font-size . "11pt"))
               (font `(,font-family ,font-size))
               (line-height '(line-height . "1.2"))
               (theme-color +org-msg-accent-color)
               (bold '(font-weight . "bold"))
               (color `(color . ,theme-color))
               (table `((margin-top . "6px") (margin-bottom . "6px")
                        (border-left . "none") (border-right . "none")
                        (border-top . "2px solid #222222")
                        (border-bottom . "2px solid #222222")
                        ))
               (ftl-number `(,color ,bold (text-align . "left")))
               (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                               fundamental ini json makefile man org plantuml
                               python sh xml))
               (inline-src `((background-color . "rgba(27,31,35,.05)")
                             (border-radius . "3px")
                             (padding . ".2em .4em")
                             (font-size . "90%") ,monospace-font
                             (margin . 0)))
               (code-src
                (mapcar (lambda (mode)
                          `(code ,(intern (concat "src src-" (symbol-name mode)))
                            ,inline-src))
                        inline-modes))
               (base-quote '((padding-left . "5px") (margin-left . "10px")
                             (margin-top . "20px") (margin-bottom . "0")
                             (font-style . "italic") (background . "#f9f9f9")))
               (quote-palette '("#6A8FBF" "#bf8f6a" "#6abf8a" "#906abf"
                                "#6aaebf" "#bf736a" "#bfb66a" "#bf6a94"
                                "#6abf9b" "#bf6a7d" "#acbf6a" "#6a74bf"))
               (quotes
                (mapcar (lambda (x)
                          (let ((c (nth x quote-palette)))
                            `(div ,(intern (format "quote%d" (1+ x)))
                              (,@base-quote
                               (color . ,c)
                               (border-left . ,(concat "3px solid "
                                                       (org-msg-lighten c)))))))
                        (number-sequence 0 (1- (length quote-palette))))))
          `((del nil ((color . "grey") (border-left . "none")
                      (text-decoration . "line-through") (margin-bottom . "0px")
                      (margin-top . "10px") (line-height . "11pt")))
            (a nil (,color (text-decoration . "underline dotted")))
            (a reply-header ((color . "black") (text-decoration . "none")))
            (div reply-header ((padding . "3.0pt 0in 0in 0in")
                               (border-top . "solid #e1e1e1 1.0pt")
                               (margin-bottom . "20px")))
            (span underline ((text-decoration . "underline")))
            (li nil (,line-height (margin-bottom . "0px")
                                  (margin-top . "8px")
                                  (max-width . "47em")))
            (nil org-ul ((list-style-type . "disc")))
            (nil org-ol (,@font ,line-height (margin-bottom . "18px")
                                (margin-top . "18px") (margin-left . "34px")
                                (padding-top . "0px") (padding-left . "5px")))
            (nil signature (,@font (margin-bottom . "20px")))
            (blockquote nil ((padding . "2px 12px") (margin-left . "10px")
                             (margin-top . "10px") (margin-bottom . "0")
                             (border-left . "3px solid #ccc")
                             (font-style . "italic")
                             (background . "#f9f9f9")))
            (p blockquote  ((margin . "0") (padding . "4px 0")))
            ,@quotes
            (code nil (,font-size ,monospace-font (background . "#f9f9f9")))
            ,@code-src
            (nil linenr ((padding-right . "1em")
                         (color . "black")
                         (background-color . "#aaaaaa")))
            (pre nil ((line-height . "1.2")
                      (color . ,(face-foreground 'default))
                      (background-color . ,(face-background 'default))
                      (margin . "4px 0px 8px 0px")
                      (padding . "8px 12px")
                      (width . "max-content")
                      (min-width . "50em")
                      (border-radius . "5px")
                      (font-size . "0.9em")
                      (font-weight . "500")
                      ,monospace-font))
            (div org-src-container ((margin-top . "10px")))
            (nil figure-number ,ftl-number)
            (nil table-number)
            (caption nil ((text-align . "left")
                          (background . ,theme-color)
                          (color . "white")
                          ,bold))
            (nil t-above ((caption-side . "top")))
            (nil t-bottom ((caption-side . "bottom")))
            (nil listing-number ,ftl-number)
            (nil figure ,ftl-number)
            (nil org-src-name ,ftl-number)
            (img nil ((vertical-align . "middle")
                      (max-width . "100%")))
            (img latex-fragment-inline ((margin . "0 0.1em")))
            (table nil (,@table ,line-height (border-collapse . "collapse")))
            (th nil ((border . "none") (border-bottom . "1px solid #222222")
                     (background-color . "#EDEDED") (font-weight . "500")
                     (padding . "3px 10px")))
            (td nil (,@table (padding . "1px 10px")
                             (background-color . "#f9f9f9") (border . "none")))
            (td org-left ((text-align . "left")))
            (td org-right ((text-align . "right")))
            (td org-center ((text-align . "center")))
            (kbd nil ((border . "1px solid #d1d5da") (border-radius . "3px")
                      (box-shadow . "inset 0 -1px 0 #d1d5da")
                      (background-color . "#fafbfc") (color . "#444d56")
                      (font-size . "0.85em")
                      (padding . "1px 4px") (display . "inline-block")))
            (div outline-text-4 ((margin-left . "15px")))
            (div outline-4 ((margin-left . "10px")))
            (h4 nil ((margin-bottom . "0px") (font-size . "11pt")))
            (h3 nil ((margin-bottom . "0px")
                     ,color (font-size . "14pt")))
            (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                     ,color (font-size . "18pt")))
            (h1 nil ((margin-top . "20px") (margin-bottom . "0px")
                     ,color (font-size . "24pt")))
            (p nil ((text-decoration . "none") (line-height . "1.4")
                    (margin-top . "10px") (margin-bottom . "0px")
                    ,font-size (max-width . "50em")))
            (b nil ((font-weight . "600")))
            (div nil (,@font (line-height . "12pt"))))))

  (advice-remove 'org-html--todo #'org-msg-html--todo)
  (defun +org-msg-html--todo (orig-fun todo &optional info)
    "Format todo keywords into HTML.
This function is used as an advice function of `org-html--todo'.
- ORIG-FUN is the original function.
- TODO is a TODO keyword.
- INFO is a property list."
    (cl-flet ((rgb-to-hex (r g b)
                (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255))))
      (cl-macrolet ((add-if-exist (val lst sym)
                      `(when ,val
                         (push (cons ,sym (apply #'rgb-to-hex
                                                 (color-name-to-rgb ,val)))
                               ,lst))))
        (if org-msg-export-in-progress
            (let ((face (org-get-todo-face todo)))
              (when (and todo face)
                (let (props)
                  (add-if-exist (htmlize-face-foreground face) props 'color)
                  (add-if-exist (htmlize-face-background face) props
                                'background-color)
                  (format "<span%s>%s</span>"
                          (if props
                              (format " style=\"%s\""
                                      (org-msg-props-to-style props))
                            "")
                          todo))))
          (funcall orig-fun todo info)))))
  (advice-add 'org-html--todo :around #'+org-msg-html--todo))
