;;; my-tabbar.el --- ta                              -*- lexical-binding: t; -*-
;; Copyright (C) 2017  Ivan Yonchovski
;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>

(require 'tabbar)

(defvar my/tabbar-buffer-group-calc nil
  "Buffer group for projectile.  Should be buffer local and speed up calculation of buffer groups.")

(tabbar-mode t)

(defun my/buffer-groups ()
  "Return the list of group names BUFFER belongs to.
    Return only one group for each buffer."
  (if my/tabbar-buffer-group-calc
      (symbol-value 'my/tabbar-buffer-group-calc)
    (set (make-local-variable 'my/tabbar-buffer-group-calc)
         (cond
          ((memq major-mode '(pidgin-chat-mode)) '("Chat"))
          ((memq major-mode '(mu4e-view-mode mu4e-compose-mode mu4e-headers-mode mu4e-main-mode)) '("Mail"))
          ((memq major-mode '(helm-major-mode)) '("Helm"))
          ((memq major-mode '(term-mode)) '("Terminals"))
          ((memq major-mode '(magit-log-mode
                              magit-file-mode
                              magit-diff-mode
                              magit-revision-mode
                              magit-status-mode
                              magit-revision-mode)) '("Magit"))
          ((and (string-equal "*" (substring (s-trim (buffer-name)) 0 1))
                (not (eq major-mode 'cider-repl-mode))) '("Misc"))
          ((condition-case err
               (projectile-project-root)
             (error nil)) (list (projectile-project-name)))
          ((and (condition-case err
                    (projectile-project-root)
                  (error nil))
                (eq major-mode 'cider-repl-mode)) (list (projectile-project-name) "Test"))
          ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
          ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
          ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
          ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
          ((memq major-mode '(dired-mode)) '("Dir"))
          (t '("Main"))))
    (symbol-value 'my/tabbar-buffer-group-calc)))

(setq tabbar-buffer-groups-function 'my/buffer-groups)

(spacemacs/set-leader-keys "bn" 'tabbar-forward-tab)
(spacemacs/set-leader-keys "bp" 'tabbar-backward-tab)
(bind-key (kbd "C-<next>") 'tabbar-forward-tab)
(bind-key (kbd "C-<prior>") 'tabbar-backward-tab)

 ;; Add a buffer modification state indicator in the tab label, and place a
 ;; space around the label to make it looks less crowd.
 (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
   (setq ad-return-value
         (if (and (buffer-modified-p (tabbar-tab-value tab))
                  (buffer-file-name (tabbar-tab-value tab)))
             (concat " + " (concat ad-return-value " "))
           (concat " " (concat ad-return-value " ")))))

 ;; Called each time the modification state of the buffer changed.
 (defun ztl-modification-state-change ()
   (tabbar-set-template tabbar-current-tabset nil)
   (tabbar-display-update))

 ;; First-change-hook is called BEFORE the change is made.
 (defun ztl-on-buffer-modification ()
   (ztl-modification-state-change))

(remove-hook 'post-command-hook #'tabbar-display-update)
 ;; This doesn't work for revert, I don't know.
 ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
