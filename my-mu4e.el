(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e")
  ;; mail configuration
  (custom-set-variables '(send-mail-function (quote smtpmail-send-it)))

  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'mu4e)

  (require 'mu4e-speedbar)
  (mu4e-alert-enable-mode-line-display)

  (defun my/mu4e-go-to-inbox ()
    "Go to inbox."
    (interactive)
    (mu4e-headers-search
     (format "maildir:\"%s\"" "/INBOX")))

  (setq mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent Items"
        mu4e-trash-folder  "/Trash"
        mu4e-sent-messages-behavior 'sent
        mu4e-msg2pdf "/usr/bin/msg2pdf"
        mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                                 ("/Drafts" . ?d)
                                 ("/Trash" . ?t)
                                 ("/Sent Items" . ?s)
                                 ("/bamboo" . ?b))
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 100
        user-mail-address "ivan.yonchovski@tick42.com"
        user-full-name  "Ivan Yonchovski"
        mu4e-compose-signature nil)

  (require 'smtpmail)
  (define-key mu4e-main-mode-map "j" nil)
  (define-key mu4e-main-mode-map "i" 'mu4e~headers-jump-to-maildir)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.office365.com" 587 "ivan.yonchovski@tick42.com" nil))
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 587
        message-kill-buffer-on-exit t)

  (mu4e)

  (use-package mu4e-alert
    :ensure t
    :config
    (mu4e-alert-enable-notifications)
    (mu4e-alert-set-default-style 'libnotify)
    (setq mu4e-alert-interesting-mail-query
          (concat "maildir:/INBOX and flag:unread"))

    (alert-add-rule
     :category "mu4e-alert"
     :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
     :continue )

    ;; display stuff on modeline as well as notify
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
    (require 'mu4e-alert)))

(provide 'my-mu4e)
