(defun my/init ()
  (interactive)

  (setenv "PATH" (concat (getenv "PATH") ":~/.bin"))

  (require 'browse-url)
  (setq browse-url-browser-function 'eww-browse-url
        browse-url-generic-program "conkeror")

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    "Magit status wrap-up."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it)

  (defun my/magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer))

  (require 'magit)
  (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)


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
  (global-set-key [remap eww-follow-link] 'my/eww-follow-link)

  ;; use mobile interface
  (setq url-user-agent (concat
                        "User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) "
                        "AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7\n"))

  (setq w3m-user-agent nil)
  (setq w3m-use-cookies t)

;;  (persistent-scratch-setup-default)

  (require 'cc-mode)
  (add-hook 'emms-playlist-mode-hook #'evil-evilified-state)

  (custom-set-variables
   '(global-command-log-mode t)
   '(evil-cross-lines t)
   '(evil-move-beyond-eol t)
   '(projectile-globally-ignored-files
     (quote ("TAGS" ".lein-repl-history")))
   '(projectile-globally-ignored-directories
     (quote (".idea" ".ensime_cache" ".eunit" "target" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "target" ))))

  (global-subword-mode t))
