;;; my-tabbar.el --- ta                              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>
;; Keywords: abbrev,

(require 'tabbar)
(defun my/buffer-groups ()
  "Return the list of group names BUFFER belongs to.
    Return only one group for each buffer."

  (cond
   ((memq major-mode '(pidgin-chat-mode)) '("Chat"))
   ((memq major-mode '(mu4e-view-mode mu4e-compose-mode mu4e-headers-mode mu4e-main-mode)) '("Mail"))
   ((memq major-mode '(helm-major-mode)) '("Helm"))
   ((memq major-mode '(magit-log-mode
                       magit-file-mode
                       magit-diff-mode
                       magit-revision-mode
                       magit-status-mode
                       magit-revision-mode)) '("Magit"))
   ((and (string-equal "*" (substring (s-trim (buffer-name)) 0 1)) (not (eq major-mode 'cider-repl-mode))) '("Misc"))
   ((condition-case err
        (projectile-project-root)
      (error nil)) (list (projectile-project-name)))
   ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
   ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
   ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
   ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
   ((memq major-mode '(dired-mode)) '("Dir"))
   (t '("Main"))))

(setq tabbar-buffer-groups-function 'my/buffer-groups)
