;;; +mail.el -*- lexical-binding: t; -*-

(after! mu4e
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-mu-binary (executable-find "mu")
        mu4e-root-maildir "~/.maildir"
        mu4e-get-mail-command (concat (executable-find "mbsync") " -a")

        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        smtpmail-queue-mail t
        smtpmail-queue-dir (expand-file-name "~/.maildir/queue/cur")
        smtpmail-store-queue-variables t

        mu4e-update-interval (* 5 60)
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        mu4e-sent-messages-behavior 'delete
        ;;mu4e-sent-messages-behavior 'sent
        message-kill-buffer-on-exit t

        mu4e-personal-addresses '("bartek@rndity.com")

        mu4e-use-fancy-chars t
        mu4e-headers-visible-lines 10
        mu4e-headers-visible-columns 100
        mu4e-split-view 'vertical
        mu4e-headers-skip-duplicates t
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

  (set-email-account! "bartek@rndity.com"
                      '((user-mail-address . "bartek@rndity.com")
                        (user-full-name . "Bartłomiej Świercz")
                        (mu4e-drafts-folder . "/bartek@rndity.com/[Gmail]/Drafts")
                        (mu4e-sent-folder . "/bartek@rndity.com/[Gmail]/Sent Mail")
                        (mu4e-refile-folder . "/bartek@rndity.com/[Gmail]/All Mail")
                        (mu4e-trash-folder . "/bartek@rndity.com/[Gmail]/Bin")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-user . "bartek@rndity.com")
                        (org-msg-greeting-fmt . "

#+begin_signature
--
Bartłomiej Świercz
/Chief Executive Officer/ of *[[https://rndity.com][rnd.guru]]*

/tel: +48 603 717 633/
#+end_signature"

                                              ))
                      t)

  (setq +mu4e-gmail-accounts '(("bartek@rndity.com"     "/bartek@rndity.com")))

  (setq mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask)

  (setq mu4e-bookmarks
  '(( :name  "Unread messages"
             :query "flag:unread AND NOT flag:trashed and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
             :key ?u)
    ( :name "Today's messages"
            :query "date:today..now and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?t)
    ( :name "Last 7 days"
            :query "date:7d..now and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?w)
    ( :name "Messages with images"
            :query "mime:image/* and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?i)
    ( :name "Messages with PDFs"
            :query "mime:application/pdf and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam"
            :key ?p)
    ( :name "Meeting invitations"
            :query "file:/\.ics$/"
            :key ?m)))

  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed and not maildir:/bartek@rndity.com/[Gmail]/Bin and not maildir:/bartek@rndity.com/[Gmail]/Spam")
  )

(use-package! org-msg
  :defer t
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng d:nil")
        (setq org-msg-startup "hidestars indent inlineimages")
        (setq org-msg-default-alternatives '((new . (utf-8 html))
                                       (reply-to-text . (utf-8 html))
                                       (reply-to-html . (utf-8 html))))
        (setq org-msg-posting-style 'top-posting))
