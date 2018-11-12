;;; my-pidgin.el --- s                               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>
;; Keywords: abbrev, abbrev, abbrev,

(add-to-list 'load-path "~/.remote-config/config/pidgin/")
(require 'pidgin)
(pidgin-connect)

(defun my/buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defun my/helm-chats ()
  "Helm chats"
  (interactive)
  (helm :sources (helm-build-sync-source "REPS"
                   :candidates (-map 'buffer-name
                                     (-filter (lambda (buffer)
                                                (s-equals?
                                                 (my/buffer-mode buffer) "pidgin-chat-mode"))
                                              (buffer-list)))
                   :action '(("Switch to chat" . switch-to-buffer)
                             ("Delete buffer" . kill-buffer)
                             ("Add to Perspective" . persp-add-buffer)))
        :buffer "*chats*"))

(defun my/helm-chats-active ()
  "Helm chats"
  (interactive)
  (let ((chat-buffers (-map 'pidgin-chat-get-buffer pidgin-activity-list)))
    (case (length chat-buffers)
      (0 (my/helm-chats))
      (1 (switch-to-buffer (first chat-buffers)))
      (t (helm :sources (helm-build-sync-source "REPS"
                          :candidates chat-buffers
                          :action '(("Switch to chat" . switch-to-buffer)
                                    ("Delete buffer" . kill-buffer)
                                    ("Add to Perspective" . persp-add-buffer)))
               :buffer "*chats*")))))

(spacemacs/set-leader-keys "bc" 'my/helm-chats-active)
(spacemacs/set-leader-keys "bC" 'my/helm-chats)

(spacemacs/set-leader-keys "jc"
  (lambda () (interactive)
    (pidgin-chat-with "facebook")))

(spacemacs/set-leader-keys "jC"
  (lambda () (interactive)
    (pidgin-chat-with "skype (http)")))

(spacemacs/set-leader-keys "jp" 'my/chat-with-petya)

(defun my/chat-with-petya ()
  (interactive)
  (let* ((user "Петя Радева|facebook")
         (curr-buf (or (get-buffer (pidgin-chat-get-buffer user))
                       (pidgin-chat-create-buffer user))))
    (switch-to-buffer curr-buf)))

(pidgin-activity-mode t)
