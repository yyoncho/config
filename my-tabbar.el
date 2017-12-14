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

(setq tabbar-buffer-groups-function 'my/buffer-groups)

(bind-key (kbd "C-<next>") 'tabbar-forward-tab)
(bind-key (kbd "C-<prior>") 'tabbar-backward-tab)
;; global leader key configuration
(spacemacs/set-leader-keys "bn" 'tabbar-forward-tab)
(spacemacs/set-leader-keys "bp" 'tabbar-backward-tab)

(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))

(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))

(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))

(global-set-key [(control tab)] 'shk-tabbar-next)
(global-set-key [(control shift tab)] 'shk-tabbar-prev)

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
   (set-buffer-modified-p t)
   (ztl-modification-state-change))


 (add-hook 'after-save-hook 'ztl-modification-state-change)

 ;; This doesn't work for revert, I don't know.
 ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
 (add-hook 'first-change-hook 'ztl-on-buffer-modification)
