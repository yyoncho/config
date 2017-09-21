(eval-after-load "cider"
  '(progn
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
                 ("(\\(fn\\)[\[[:space:]]"  ; anon funcs 1
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "λ")
                            nil)))
                 ("(\\(not=\\)[\[[:space:]]"  ; anon funcs 1
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "≠")
                            nil)))
                 ("(\\(def\\)[\[[:space:]]"  ; anon funcs 1
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "≡")
                            nil)))
                 ("\\(#\\)("                ; anon funcs 2
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "λ")
                            nil)))
                 ("\\(Math/PI\\)"                ; anon funcs 2
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

     (setq cider-save-file-on-load t
           cider-auto-jump-to-error nil
           cider-auto-select-test-report-buffer nil
           cider-show-error-buffer nil
           cider-lein-command "~/.bin/lein"
           cider-prompt-save-file-on-load t
           cider-use-fringe-indicators t)

     (add-hook 'cider-mode-hook 'rainbow-delimiters-mode-enable)

     (defun cider-emit-interactive-eval-err-output (output)
       "Emit err OUTPUT resulting from interactive code evaluation.
The output can be send to either a dedicated output buffer or the current
REPL buffer.  This is controlled via
`cider-interactive-eval-output-destination'."
       (my/show-error output)
       (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-stderr))

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
         "tv" 'cider-toggle-trace-var
         "tg" 'cider-test-rerun-test
         "te" 'cider-visit-error-buffer
         "nt" 'cider-toggle-trace-ns
         "j"  'evil-operator-clojure
         "es" 'my/mount-restart
         "ld" 'my/timbre-debug
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
         "ej" 'cider-pprint-eval-last-sexp))

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

     (defun my/mount-restart ()
       "Restarts mount"
       (interactive)
       (my/exec-clj-code "(do (mount/stop) (mount/start))"))

     (defun my/timbre-debug ()
       "Change level to debug"
       (interactive )
       (my/exec-clj-code "(taoensso.timbre/set-level! :debug)"))

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
       (helm :sources (helm-build-sync-source "REPS"
                        :candidates (-map 'buffer-name
                                          (-filter (lambda (buffer)
                                                     (s-equals?
                                                      (buffer-mode buffer) "cider-repl-mode"))
                                                   (buffer-list)))
                        :action '(("Switch to REPL" . switch-to-buffer)
                                  ("Kill" . (lambda (candidate)
                                              (interactive)
                                              (with-current-buffer candidate
                                                (cider-quit))))))
             :buffer "*helm sync source*"))))
