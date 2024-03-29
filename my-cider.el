((progn

   (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
   (add-hook 'clojure-mode-hook (lambda  ()
                                  (setq projectile-type 'clojure-cli))
             )

   (dolist (mode '(clojure-mode clojurescript-mode cider-mode clojurec-mode))
     (eval-after-load mode
       (font-lock-add-keywords
        mode '(("(\\(defn\\)[\[[:space:]]" ; anon funcs 1
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "ƒ")
                          nil)))
               ("(\\(defmacro\\)[\[[:space:]]"
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "µ")
                          nil)))
               ("(\\(fn\\)[\[[:space:]]" ; anon funcs 1
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "λ")
                          nil)))
               ("(\\(not=\\)[\[[:space:]]" ; anon funcs 1
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "≠")
                          nil)))
               ("(\\(def\\)[\[[:space:]]" ; anon funcs 1
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "≡")
                          nil)))
               ("\\(#\\)("              ; anon funcs 2
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "λ")
                          nil)))
               ("\\(Math/PI\\)"         ; anon funcs 2
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "π")
                          nil)))
               ("(\\(partial\\)[\[[:space:]]"
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "Ƥ"))))
               ("(\\(comp\\)[\[[:space:]]"
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "∘"))))
               ("\\(#\\)("
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "ƒ"))))
               ("\\(#\\){"
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "∈"))))))))
   (require 'cider)

   (setq cider-save-file-on-load t
         cider-auto-jump-to-error nil
         cider-auto-select-test-report-buffer t
         cider-show-error-buffer t
         cider-lein-command "lein"
         cider-prompt-save-file-on-load t
         cider-use-fringe-indicators t
         clojure-enable-fancify-symbols t
         clojure-indent-style :align-arguments
         clojure-align-forms-automatically t
         cider-dynamic-indentation nil
         cider-cljs-lein-repl 'node)

   (add-hook 'cider-mode-hook 'rainbow-delimiters-mode-enable)
   (add-hook 'cider-mode-hook 'helm-cider-mode)
   (add-hook 'cider-mode-hook 'aggressive-indent-mode)

   (bind-key "TAB" 'company-indent-or-complete-common cider-mode-map)

   (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
     "sc" 'cider-repl-clear-buffer)

   (defun my/cycle-log-level ()
     (interactive)
     (save-mark-and-excursion
       (forward-word)
       (re-search-backward "debug\\|info")
       (replace-match
        (pcase (thing-at-point 'word)
          ("debug" "info")
          ("info"  "debug")))))

   (require 'helm-cider)

   ;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
   (add-hook 'clojure-mode-hook 'flycheck-mode)

   (dolist (m '(clojure-mode
                clojurec-mode
                clojurescript-mode
                clojurex-mode
                cider-repl-mode
                cider-clojure-interaction-mode))

     (spacemacs/set-leader-keys-for-major-mode m
       "(" 'clojure-convert-collection-to-list
       "[" 'clojure-convert-collection-to-vector
       "{" 'clojure-convert-collection-to-map
       "ea" 'cider-load-all-project-ns
       "ep" 'cider-pprint-eval-defun-at-point
       "qr" 'cider-restart
       "el" 'my/cider-eval-end-of-line
       "rcl" 'my/cycle-log-level
       "tv" 'cider-toggle-trace-var
       "tg" 'cider-test-rerun-test
       "df" 'cider-debug-defun-at-point
       "te" 'cider-visit-error-buffer
       "nt" 'cider-toggle-trace-ns
       "j"  'evil-operator-clojure
       "es" 'my/mount-restart
       "ld" 'my/timbre-debug
       ;; "sc" (lambda () (cider-find-and-clear-repl-output t))
       "li" 'my/timbre-info
       "lt" 'my/timbre-trace
       "," 'cider-eval-defun-at-point
       "dl" 'cider-inspect-last-result
       "ey" (lambda ()
              (interactive)
              (cider-read-and-eval (current-kill 1 t)))
       "k" 'cider-interrupt
       ";" 'sp-comment
       "fp" 'my/find-project-file
       "ej" 'cider-pprint-eval-last-sexp
       "F" 'my/fix-version))

   (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode "gu" 'cider-jump-to-locref-at-point)
   (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode "sc" 'cider-repl-clear-buffer)

   (evil-define-operator evil-operator-clojure (beg end)
     "Evil operator for evaluating code."
     :move-point nil
     (interactive "<r>")
     (cider-eval-region beg end))

   (defun my/exec-clj-code (form)
     "Exec clj code"
     (let* ((override cider-interactive-eval-override)
            (ns-form (if (cider-ns-form-p form) "" (format "(ns %s)" (cider-current-ns)))))
       (with-current-buffer (get-buffer-create cider-read-eval-buffer)
         (erase-buffer)
         (clojure-mode)
         (unless (string= "" ns-form)
           (insert ns-form "\n\n"))
         (insert form)
         (let ((cider-interactive-eval-override override))
           (cider-interactive-eval form)))))

   ;; (require 'sayid) ;; (setq sayid-inject-dependencies-at-jack-in nil)

   (defun my/mount-restart ()
     "Restarts mount"
     (interactive)
     (my/exec-clj-code "(do (mount/stop) (mount/start))"))

   (defun my/timbre-debug ()
     "Change level to debug"
     (interactive )
     (my/exec-clj-code "(taoensso.timbre/set-level! :debug)"))

   ;; clojurescript-mode
   (defun my/cider-run-tests-in-current-ns ()
     "docstring"
     (interactive)
     (my/exec-clj-code "(cljs.test/run-tests *ns*)"))

   (spacemacs/set-leader-keys-for-major-mode
     'clojurescript-mode "tn" 'my/cider-run-tests-in-current-ns)

   (defun my/timbre-info ()
     "Change level to info"
     (interactive)
     (my/exec-clj-code "(taoensso.timbre/set-level! :info)"))

   (defun my/timbre-trace ()
     "Change level to info"
     (interactive)
     (my/exec-clj-code "(taoensso.timbre/set-level! :trace)"))

   (defun my/list-repls ()
     (interactive)
     (let ((buffers (-map 'buffer-name
                          (-filter (lambda (buffer)
                                     (s-equals?
                                      (buffer-mode buffer) "cider-repl-mode"))
                                   (buffer-list)))))
       (case (length buffers)
         (0 (error "There is no active repls"))
         (1 (switch-to-buffer (first buffers)))
         (t (helm :sources (helm-build-sync-source "REPS"
                             :candidates buffers
                             :action '(("Switch to REPL" . switch-to-buffer)
                                       ("Kill" . (lambda (candidate)
                                                   (interactive)
                                                   (with-current-buffer candidate
                                                     (cider-quit))))
                                       ("Add to Perspective" . (lambda (candidate)
                                                                 (interactive)
                                                                 (persp-add-buffer candidate)))))
                  :buffer "*helm sync source*"))))
     (add-hook 'cider-mode-hook (lambda ()
                                  (interactive)
                                  (flycheck-mode nil))))

   (require 'clj-refactor)
   (setq cljr-warn-on-eval nil)

   (setq cider-auto-select-test-report-buffer nil)

   (setq nrepl-prompt-to-kill-server-buffer-on-quit nil)
   (defun my/find-project-file (args)
     "Find file in upper dirs"
     (interactive "P")
     (if-let* ((pf (expand-file-name
                    (concat (locate-dominating-file
                             (if (string= (file-name-nondirectory (buffer-file-name)) "project.clj")
                                 (file-name-directory
                                  (directory-file-name (file-name-directory (buffer-file-name))))
                               (buffer-file-name))
                             "project.clj")
                            "project.clj"))))
         (find-file pf)
       (message "Unable to find project.clj")))

   (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
   (spacemacs/set-leader-keys "bl" 'my/list-repls)
   (defun my/cider-eval-end-of-line ()
     "Evaluate the last sexp at the end of the current line."
     (interactive)
     (save-excursion
       (end-of-line)
       (cider-eval-last-sexp)))) eval-after-load
       '"cider")
