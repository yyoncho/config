;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.remote-config/config/layers/")
   dotspacemacs-configuration-layers
   '(
     vinegar
     imenu-list
     lua
     evil-cleverparens
     spacemacs
     spacemacs-base
     spacemacs-bootstrap
     version-control
     haskell
     csv
     windows-scripts
     sql
     yaml
     html
     typescript
     mu4e
     fasd
     colors
     shell
     eww
     javascript
     python
     racket
     helm
     auto-completion
     spacemacs-layouts
     better-defaults
     evil-commentary
     emacs-lisp
     git
     c-c++
     markdown
     org
     spell-checking
     syntax-checking
     ibuffer
     clojure
     command-log
     my-layer
     elfeed)
   dotspacemacs-additional-packages
   '(java-snippets
     zonokai-theme
     w3m
     emms
     auto-complete-nxml
     sr-speedbar
     meghanada
     forecast
     evil-smartparens
     excorporate
     cypher-mode
     org-jira
     dired-efap
     dired+
     easy-kill
     ace-link
     java-snippets
     sx
     midje-mode
     eww
     persistent-scratch
     diff-hl
     helm-dash
     dired-subtree
     emms
     flycheck-clojure)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(zonokai-blue)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro Medium"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-l"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.1
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 100
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters;; jira
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis 'all
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'current
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used  for now. (default nil)
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ())

(defun dotspacemacs/user-config ()
  (interactive)
  (require 'eww)
  (define-key eww-mode-map "f" 'ace-link-eww)
  (define-key eww-mode-map "g" 'eww)
  (define-key eww-mode-map "r" 'eww)
  (define-key eww-mode-map "p" 'eww-back-url)
  (define-key eww-mode-map "n" 'eww-forward-url)
  (define-key eww-mode-map "G" 'eww-reload)
  (define-key eww-mode-map "h" 'helm-eww-history)
  (define-key eww-mode-map "l" 'helm-eww-links)

  (setq helm-ff-guess-ffap-filenames t)

  ;; do not ask when deleting buffers
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ;; contrast configuration
  (require 'shr-color)
  (setq shr-color-visible-distance-min 62)
  (setq shr-color-visible-luminance-min 80)

  (smartparens-global-strict-mode t)
  (evil-visual-mark-mode t)
  (setq custom-file "~/.remote-config/config/.custom.el")
  ;; (load custom-file)

  (spacemacs/set-leader-keys "jj" 'my/goto-char-3)
  (spacemacs/set-leader-keys "xts" 'transpose-sexps)
  (spacemacs/set-leader-keys "oo" 'recentf-open-most-recent-file)
  (spacemacs/set-leader-keys "od" 'my/duplicate-2)
  (spacemacs/set-leader-keys "ot" 'projectile-find-test-file)
  (spacemacs/set-leader-keys "pp" 'my/projectile-switch-project-dired)
  (spacemacs/set-leader-keys "pt" 'projectile-test-project)
  (spacemacs/set-leader-keys "pT" 'neotree-find-project-root)
  (spacemacs/set-leader-keys "oP" 'spacemacs/paste-transient-state/evil-paste-after)
  (spacemacs/set-leader-keys "op" 'spacemacs/paste-transient-state/evil-paste-before)

  (sp-use-paredit-bindings)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-automatic-symbol-highlight-on)
  (set powerline-default-separator 'alternate)
  (setf confluence-url "https://confluence.tick42.com:8443/rpc/xmlrpc")

  (require 'cider)
  (require 'clojure-mode)

  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode
               cider-repl-mode
               cider-clojure-interaction-mode))

    (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                        m (car x) (cdr x)))
          cider--key-binding-prefixes)

    (spacemacs/set-leader-keys-for-major-mode m
      "(" 'clojure-convert-collection-to-list
      "[" 'clojure-convert-collection-to-vector
      "{" 'clojure-convert-collection-to-map
      "ea" 'cider-load-all-project-ns
      "ep" 'cider-pprint-eval-defun-at-point
      "qr" 'cider-restart
      "tv" 'cider-toggle-trace-var
      "tg" 'cider-test-rerun-test
      "nt" 'cider-toggle-trace-ns
      "j"  'evil-operator-clojure
      "es" 'my/mount-restart
      "ld" 'my/timbre-debug
      "li" 'my/timbre-info
      "," 'cider-eval-defun-at-point
      "dl" 'cider-inspect-last-result
      "k" 'cider-interrupt
      ";" 'sp-comment
      "fp" 'my/find-project-file
      "ej" 'cider-pprint-eval-last-sexp))

  ;; org jira key bindings
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jp" 'org-jira-progress-issue)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ji" 'org-jira-update-issue)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jg" 'org-jira-get-issues)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jl" 'org-jira-update-worklogs-from-org-clocks)

  ;; Emacs lisp
  (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "," 'eval-defun)

  (require 'evil-smartparens)
  (setq emmet-self-closing-tag-style "")
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "[" "]" :wrap "M-[")
  (global-evil-mc-mode t)
  (setq-default which-key-idle-delay 2.0)
  (setq-default which-key-idle-secondary-delay 1.0)
  (evil-define-operator evil-operator-clojure (beg end)
    "Evil operator for evaluating code."
    :move-point nil
    (interactive "<r>")
    (cider-eval-region beg end))


  (evil-define-operator evil-operator-duplicate (beg end)
    "Duplicate action."
    :move-point nil
    (interactive "<r>")

    (save-excursion
      (kill-ring-save beg end)
      (goto-char end)
      (newline-and-indent)
      (yank)))

  (evil-define-operator my/evil-operator-paste (beg end)
    "Paste action."
    :move-point nil
    (interactive "<r>")
    (save-excursion
      (goto-char end)
      (insert " ")
      (yank)))

  (setq cider-save-file-on-load t)

  (bind-key ";" 'sp-comment)
  (spacemacs/set-leader-keys "bb" 'helm-buffers-list)
  (spacemacs/set-leader-keys "d" 'evil-operator-duplicate)
  (spacemacs/set-leader-keys "ga" 'my/magit-stage-modified)
  (spacemacs/set-leader-keys "gc" 'magit-commit)
  (spacemacs/set-leader-keys "gwc" 'magit-wip-commit)
  (spacemacs/set-leader-keys "gwl" 'magit-wip-log)
  (setq git-commit-summary-max-length 999)
  (setq kill-do-not-save-duplicates t)
  (setq cider-lein-command "~/.bin/lein")

  ;; disable backup files
  (setq make-backup-files nil)

  (global-diff-hl-mode t)
  (add-hook 'cider-mode-hook 'rainbow-delimiters-mode-enable)

  (spacemacs|define-jump-handlers java-mode meghanada-jump-declaration)

  (remove-hook 'cider-mode-hook 'aggressive-indent-mode)

  (global-subword-mode t)
  (require 'dired)
  (require 'dired+)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-deletes 'always)
  (setq dired-deletion-confirmer '(lambda (x) t))

  (my/init)

  (defun my/switch-to-compilation-buffer (arg)
    "Switch to compilation buffer"
    (interactive "P")
    (switch-to-buffer "*compilation*"))

  (spacemacs/set-leader-keys "cb" 'my/switch-to-compilation-buffer)

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
  (global-flycheck-mode t)

  (setq clojure-enable-fancify-symbols t)
  (setq-default dired-listing-switches "-aBhl  --group-directories-first")
  (setq cider-save-file-on-load t)
  (defun my/find-project-file (args)
    "Find file in upper dirs"
    (interactive "P")
    (if-let ((pf (expand-file-name
                  (concat (locate-dominating-file
                           (if (string= (file-name-nondirectory (buffer-file-name)) "project.clj")
                               (file-name-directory
                                (directory-file-name (file-name-directory (buffer-file-name))))
                             (buffer-file-name))
                           "project.clj")
                          "project.clj"))))
        (find-file pf)
      (message "Unable to find project.clj")))
  (setq clojure-indent-style :align-arguments
        clojure-align-forms-automatically t)

  (defun my/projectile-switch-project-dired (&optional arg)
    "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
    (interactive "P")
    (let (projects)
      (if (setq projects (projectile-relevant-known-projects))
          (projectile-completing-read
           "[Dired]Switch to project: " projects
           :action (lambda (project)
                     (dired project)))
        (error "There are no known projects"))))

  (custom-set-faces
   '(clojure-interop-method-face ((t (:foreground "darkgray"))))
   '(company-preview-common ((t (:background "dark gray"))))
   '(company-preview-search ((t (:background "dark gray"))))
   '(diredp-compressed-file-suffix ((t (:foreground "red"))))
   '(ediff-even-diff-A ((t (:background "dim gray"))))
   '(ediff-even-diff-B ((t (:background "dim gray"))))
   '(ediff-even-diff-C ((t (:background "dim gray"))))
   '(ediff-odd-diff-A ((t (:background "dim gray"))))
   '(ediff-odd-diff-B ((t (:background "dim gray"))))
   '(ediff-odd-diff-C ((t (:background "dim gray"))))
   '(evil-search-highlight-persist-highlight-face ((t (:inherit lazy-highlight :background "dim gray"))))
   '(font-lock-builtin-face ((t (:foreground "orange red" :weight bold))))
   '(region ((t (:background "dim gray" :foreground "#d8d8d8")))))

  (define-key evil-normal-state-map "p" 'evil-paste-before)
  (define-key evil-normal-state-map "P" 'evil-paste-after)

  (setq projectile-create-missing-test-files t)
  (setq cljr-warn-on-eval nil)

  (evil-define-command my/goto-end-of-form (count)
    "Go to end the the form."
    (interactive "<c>")
    (let ((line-end (point-at-eol)))
      (when (or (when (sp-up-sexp count) (backward-char) t)
                (-when-let (enc-end (cdr (evil-cp--top-level-bounds)))
                  (goto-char (1- enc-end))))
        (if (<= (point) line-end)
            (evil-insert 1)
          (evil-insert 1)))))

  (require 'evil-cleverparens)

  (global-set-key [remap evil-cp-end-of-defun] 'my/goto-end-of-form)

  (diredp-toggle-find-file-reuse-dir 1)

  (defun my/magit-stage-modified ()
    "Stage all changes to files"
    (interactive)
    (magit-with-toplevel
      (magit-stage-1 "--all")))

  (magit-wip-after-apply-mode t)
  (setq evil-want-fine-undo nil
        evil-cross-lines t)
  ;; jira configuration
  (setq jiralib-url "https://jira.tick42.com"
        jiralib-user-login-name "iyonchovski")
  (setq cider-prompt-save-file-on-load t)
  (setq cider-use-fringe-indicators t)
  (setq-default dotspacemacs-configuration-layers
                '((clojure :variables clojure-enable-fancify-symbols t)))
  (mu4e-alert-enable-mode-line-display)
  (setq evil-lisp-state-enter-lisp-state-on-command nil)

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

  (require 'auto-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  (define-key company-active-map (kbd "<escape>") 'company-abort)

  (require 'company)
  (define-key ac-complete-mode-map (kbd "<escape>") 'ac-abort)
  (setq magit-diff-arguments '("--stat" "--no-ext-diff" "--ignore-all-space"))
  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))
  (setq evil-move-cursor-back nil)

  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "tt" 'meghanada-run-junit-test-case
    "tg" 'meghanada-run-junit-recent
    "ea" 'meghanada-compile-project
    "eb" 'meghanada-compile-file
    "qr" 'meghanada-restart
    "tn" 'meghanada-run-junit-class)

  (defun my/switch-full-screen ()
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -b remove,maximized_horz")
    (shell-command "wmctrl -r :ACTIVE: -b remove,maximized_vert")
    (my/two-monitors)
    (shell-command "wmctrl -r :ACTIVE: -b add,above"))

  (spacemacs/set-leader-keys "of" 'my/switch-full-screen)
  (eval-after-load 'flycheck '(flycheck-clojure-setup))
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (display-time-mode t)
  (fset 'my/duplicate-2
        (lambda (&optional arg) "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item (quote (" d2L" 0 "%d")) arg))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eww-search-prefix "https://www.google.com/search?q=")
 '(package-selected-packages
   (quote
    (typescript-mode faceup org-category-capture ht alert log4e gntp markdown-mode skewer-mode json-snatcher json-reformat js2-mode parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flyspell-correct pos-tip flycheck flx url-http-ntlm soap-client fsm ntlm magit magit-popup git-commit with-editor iedit org-plus-contrib orgit zonokai-theme yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify w3m volatile-highlights vi-tilde-fringe uuidgen use-package unfill toc-org tide tagedit sx sr-speedbar sql-indent spaceline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racket-mode pyvenv pytest pyenv-mode py-isort pug-mode powershell pip-requirements persp-mode persistent-scratch pcre2el paradox origami org-projectile org-present org-pomodoro org-jira org-download org-bullets open-junk-file neotree mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode midje-mode meghanada markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc java-snippets intero info+ indent-guide imenu-list ibuffer-projectile hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy forecast flyspell-correct-helm flycheck-pos-tip flycheck-haskell flycheck-clojure flx-ido fill-column-indicator fasd fancy-battery eyebrowse expand-region exec-path-from-shell excorporate eww-lnum evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-smartparens evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emms emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies easy-kill dumb-jump disaster dired-subtree dired-efap dired+ diff-hl define-word cython-mode cypher-mode csv-mode company-web company-tern company-statistics company-ghci company-ghc company-cabal company-c-headers company-anaconda command-log-mode column-enforce-mode color-identifiers-mode coffee-mode cmm-mode cmake-mode clojure-snippets clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu auto-yasnippet auto-highlight-symbol auto-dictionary auto-complete-nxml auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
