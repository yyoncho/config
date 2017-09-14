(defun my/init ()
  (interactive)
  (setq make-backup-files nil)

  (setenv "PATH" (concat (getenv "PATH") ":~/.bin"))

  ;; prevent prompt for killing emcacsclient buffer
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

  ;;(set-face-attribute 'default nil :height 120)


  (require 'browse-url)
  (setq browse-url-browser-function 'eww-browse-url
        browse-url-generic-program "conkeror")

  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    "Magit status wrap-up."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun my/magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

  ;; global key configuration
  (bind-key "C-x k" 'kill-current-buffer)
  (bind-key "M-j" 'evil-join)
  (bind-key "M-/" 'hippie-expand)
  (bind-key "C-h" 'backward-delete-char)
  (bind-key "C-M-h" 'backward-kill-word)

  (bind-key "C-<backspace>" 'subword-backward-kill)

  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (emms-mode-line -1)

  (defun my/projectile-switch-project-magit (&optional arg)
    "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
    (interactive "P")
    (let (projects)
      (if (setq projects (projectile-relevant-known-projects))
          (projectile-completing-read
           "[Magit]Switch to project: " projects
           :action (lambda (project)
                     (magit-status project)))
        (error "There are no known projects"))))


  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))

  (global-set-key [remap kbd-end-or-call-macro] 'my/kmacro-end-and-call-macro)

  (global-set-key [remap eww-follow-link] 'my/eww-follow-link)


  ;; use mobile interface
  (setq url-user-agent (concat
                        "User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) "
                        "AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7\n"))

  (setq w3m-user-agent nil)
  (setq w3m-use-cookies t)


  (persistent-scratch-setup-default)
  (require 'cc-mode)
  (add-hook 'java-mode-hook #'meghanada-mode)
  (add-hook 'java-mode-hook #'flycheck-mode)
  (add-hook 'emms-playlist-mode-hook #'evil-evilified-state)

  (fset 'my/copy-worklog
        [?\C-c ?\C-x ?\C-w ?\C-y ?\C-y ?\C-p tab ?\C-n tab ?\C-n ?\C-k ?\C-k ?\C-n ?\M-f ?\M-f ?\M-f ?\M-f ?\C-f ?\M-x ?m ?y ?- backspace ?/ ?i ?n tab return])

  (custom-set-variables
   '(mu4e-hide-index-messages t)
   '(bmkp-last-as-first-bookmark-file "~/.emacs.d/savefile/bookmarks")
   '(excorporate-configuration
     (quote
      ("ivan.yonchovski@tick42.com" . "https://pod51036.outlook.com/ews/Exchange.asmx")))
   '(global-auto-highlight-symbol-mode t)
   '(global-command-log-mode t)
   '(evil-cross-lines t)
   '(evil-move-beyond-eol t)
   '(projectile-globally-ignored-files
     (quote ("TAGS" ".lein-repl-history")))
   '(projectile-globally-ignored-directories
     (quote (".idea" ".ensime_cache" ".eunit" "target" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "target" ))))

  (global-subword-mode t)
  (setq evil-move-beyond-eol t)

  (load "soap-client.el"))
