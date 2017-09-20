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
   '(javascript
     vinegar
     jabber
     imenu-list
     evil-cleverparens
     spacemacs
     (java :variables java-backend 'meghanada)

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
     python
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
     w3m
     command-log
     my-layer
     elfeed
     semantic)
   dotspacemacs-additional-packages
   '(java-snippets
     flash-region
     evil-textobj-anyblock
     autopair
     company-lsp
     lsp-java
     zonokai-theme
     ecukes
     feature-mode
     emms
     skype
     evil-smartparens
     excorporate
     cypher-mode
     org-jira
     dired-efap
     dired+
     sx
     eww
     persistent-scratch
     diff-hl
     helm-dash
     dired-subtree
     flycheck-clojure
     all-the-icons
     all-the-icons-dired
     yahoo-weather)
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
   dotspacemacs-helm-use-fuzzy 'source
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.1
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
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

  (setq helm-projectile-fuzzy-match nil)

  (require 'helm-bookmark)
  (require 'eww)
  ;; eww configuration
  (add-hook 'eww-mode-hook #'evil-evilified-state)

  (setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

  (define-key eww-mode-map "f" 'ace-link-eww)
  (define-key eww-mode-map "g" 'eww)
  (define-key eww-mode-map "r" 'eww)
  (define-key eww-mode-map "p" 'eww-back-url)
  (define-key eww-mode-map "n" 'eww-forward-url)
  (define-key eww-mode-map "G" 'eww-reload)
  (define-key eww-mode-map "h" 'helm-eww-history)
  (define-key eww-mode-map "l" 'helm-eww-links)

  (spacemacs/toggle-spelling-checking-off)

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


  (sp-use-paredit-bindings)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-automatic-symbol-highlight-on)
  (set powerline-default-separator 'alternate)
  (setf confluence-url "https://confluence.tick42.com:8443/rpc/xmlrpc")

  (spacemacs/toggle-spelling-checking-off)

  ;; org jira key bindings
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jp" 'org-jira-progress-issue)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ja" 'org-jira-assign-issue)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jg" 'org-jira-get-issue)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ji" 'org-jira-update-issue)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jG" 'org-jira-get-issues)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jr" 'org-jira-refresh-issue)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jR" 'org-jira-refresh-issues-in-buffer)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jc" 'org-jira-update-comment)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jd" 'org-jira-download-attachment)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jt" 'org-jira-todo-to-jira)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "jl" 'org-jira-update-worklogs-from-org-clocks)

  ;; Emacs lisp
  (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "," 'eval-defun)

  (c-set-offset 'substatement-open 0)
  (setenv "EMACS" (expand-file-name "~/.bin/emacs/bin/emacs-25.2"))
  (require 'evil-smartparens)
  (setq emmet-self-closing-tag-style "")
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "[" "]" :wrap "M-[")

  ;; (setq-default grep-find-ignored-directories (add-to-list 'grep-find-ignored-directories "target"))
  ;; (setq-default helm-grep-ignored-directories (add-to-list 'grep-find-ignored-directories "target"))

  ;; evil-mc configuration
  (global-evil-mc-mode t)

  ;; which key configuration
  (setq-default which-key-idle-delay 1.0
                which-key-idle-secondary-delay 0.1)

  (evil-define-operator evil-operator-duplicate (beg end)
    "Duplicate action."
    :move-point nil
    (interactive "<r>")

    (save-excursion
      (kill-ring-save beg end)
      (goto-char end)
      (newline-and-indent)
      (yank)))

  (evil-define-operator my/evil-replace-with-kill-ring (beg end)
    "Replace with killring action."
    :move-point nil
    (interactive "<r>")

    (save-excursion
      (delete-region beg end)
      (call-interactively 'evil-paste-before 1)))

  (setq git-commit-summary-max-length 999
        kill-do-not-save-duplicates t

        ;; disable backup files
        make-backup-files nil)

  (global-diff-hl-mode t)

  (global-subword-mode t)

  (my/init)

  (defun my/switch-to-compilation-buffer (arg)
    "Switch to compilation buffer"
    (interactive "P")
    (switch-to-buffer "*compilation*"))

  (global-flycheck-mode t)

  (setq clojure-enable-fancify-symbols t)

  (defun my/find-pom-file ()
    "Find file in upper dirs"
    (interactive)
    (if-let ((pf (expand-file-name
                  (concat (locate-dominating-file
                           (if (string= (file-name-nondirectory (buffer-file-name)) "pom.xml")
                               (file-name-directory
                                (directory-file-name (file-name-directory (buffer-file-name))))
                             (buffer-file-name))
                           "pom.xml")
                          "pom.xml"))))
        (find-file pf)
      (message "Unable to find pom.xml")))

  (require 'semantic)
  (global-semantic-idle-summary-mode -1)
  (global-evil-surround-mode 1)
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-position 'right)

  (setq clojure-indent-style :align-arguments
        clojure-align-forms-automatically t)

  (global-flycheck-mode -1)

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (delete 'company-capf company-backends)))

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

  (require 'company)
  (setq company-backends (-remove-item 'company-capf company-backends))

  ;; projectile
  (require 'projectile)
  (add-to-list 'projectile-globally-ignored-directories ".cask")
  (setq projectile-create-missing-test-files t)

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

  ;; general emacs configuration

  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; emms configuration
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (emms-mode-line -1)

  (require 'magit)
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
  (setq-default dotspacemacs-configuration-layers
                '((clojure :variables clojure-enable-fancify-symbols t)))

  (setq evil-lisp-state-enter-lisp-state-on-command nil)


  (require 'auto-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  (require 'company)
  (define-key company-active-map (kbd "<escape>") 'company-abort)
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
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (persistent-scratch-setup-default)

  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t)

  (defun my/add-throws ()
    (interactive)
    (save-mark-and-excursion
     (c-beginning-of-defun)
     (re-search-forward "{")
     (re-search-backward ")")
     (forward-char)
     (insert-string " throws Exception ")))

  (defun my/rename-current-method ()
    (interactive)
    (c-beginning-of-defun)
    (re-search-forward "{")
    (re-search-backward "(")
    (backward-sexp))

  (defun my/find-function-bound-to-key ()
    "Find function bound to key."
    (interactive)
    (find-function (lookup-key (current-global-map)
                               (read-key-sequence "Enter key sequence:"))))

  (setq recentf-exclude '("/tmp/meghanada-*"))

  (add-hook 'xml-mode-hook 'web-mode)

  (spacemacs/set-leader-keys-for-major-mode 'web-mode
    "fp" 'my/find-pom-file)

  (defun my/switch-full-screen ()
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -b remove,maximized_horz")
    (shell-command "wmctrl -r :ACTIVE: -b remove,maximized_vert")
    (my/two-monitors)
    (shell-command "wmctrl -r :ACTIVE: -b add,above"))

  (add-hook 'view-mode-hook 'evil-evilified-state)

  (eval-after-load 'flycheck '(flycheck-clojure-setup))
  (remove-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook (lambda () (eval-sexp-fu-flash-mode -1)))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (display-time-mode t)
  (setq helm-exit-idle-delay 0)

  (fset 'my/duplicate-2
        (lambda (&optional arg) "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item (quote (" d2L" 0 "%d")) arg)))


                                        ;(setq company-frontends (list 'helm-company))
  (setq company-auto-complete nil)
  (setq company-tooltip-idle-delay 2)
  (setq company-auto-complete-chars nil)
  (setq-default company-auto-complete nil)

  (fset 'my/format-defun
        (lambda (&optional arg) "Keyboard macro." (interactive "p")
          (save-mark-and-excursion
           (kmacro-exec-ring-item (quote ("vad j=" 0 "%d")) arg))))

  (defun my/show-error (text)
    "Shows error message"
    (interactive)
    (message (propertize (s-replace "\n" "" text) 'face 'cider-test-failure-face)))

  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; weather
  ;;(yahoo-weather-mode t)
  (setq yahoo-weather-location "Sofia")
  (setq yahoo-weather-format "|%(weather) %(temperature)C|")

  ;; indent mode
  (indent-guide-global-mode t)

  ;; better window splitting
  (defun my/vsplit-last-buffer (prefix)
    "Split the window vertically and display the previous buffer.
PREFIX - whether to switch to the other window."
    (interactive "p")
    (split-window-vertically)
    (other-window 1 nil)
    (if (= prefix 1)
        (switch-to-next-buffer)))

  (defun my/hsplit-last-buffer (prefix)
    "Split the window horizontally and display the previous buffer.
PREFIX - whether to switch to the other window."
    (interactive "p")
    (split-window-horizontally)
    (other-window 1 nil)
    (if (= prefix 1) (switch-to-next-buffer)))


  (require 'term)
  (define-key term-raw-map  (kbd "C-'") 'term-line-mode)
  (define-key term-mode-map (kbd "C-'") 'term-char-mode)
  (define-key term-raw-map  (kbd "C-y") 'term-paste)

  (global-set-key [remap kbd-end-or-call-macro] 'my/kmacro-end-and-call-macro)
  (global-set-key [remap split-window-right] 'my/hsplit-last-buffer)

  (put 'set-goal-column 'disabled nil)

  ;; enable shift selection mode
  (setq shift-selection-mode t)

  ;; java configuration
  (defun my/configure-java ()
    "Configure java"
    (interactive)
    (c-set-offset 'arglist-cont-nonempty '++)
    (c-set-offset 'arglist-intro '++)
    (electric-layout-mode t)
    (auto-complete-mode t)
    (rainbow-delimiters-mode-enable)
    (indent-guide-mode t)
    (setq c-basic-offset 4))

  (setq c-default-style
        '((java-mode . "java")
          (other . "gnu")))

  ;; company key configuration
  (require 'company)
  (define-key company-active-map "\C-p" 'company-select-previous)
  (define-key company-active-map "\C-n" 'company-select-next)
  (define-key company-active-map "\C-j" 'company-complete-selection)


  ;; always follow symlinks

  (setq vc-follow-symlinks t)

  (require 'vc-dispatcher)
  (setq vc-suppress-confirm nil)

  ;; Save point position between sessions
  (require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))

  ;; cleverparens configuration
  (spacemacs/toggle-evil-cleverparens-on)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda () (semantic-mode nil)))

  ;; set frame name to emacs
  (defun my/set-frame-name (frame)
    (modify-frame-parameters frame
                             (list (cons 'name "emacs"))))
  (add-to-list 'after-make-frame-functions 'my/set-frame-name)

  (add-hook 'yas-before-expand-snippet-hook (lambda () (autopair-mode 1)))
  (add-hook 'yas-after-exit-snippet-hook (lambda () (autopair-mode -1)))

  (defun my/two-monitors ()
    "Set frame size to cover 2 monitors"
    (interactive)
    (setq frame-resize-pixelwise t)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame) (* 2 1920) 1080 t))


  (require 'sx-interaction)
  (setq sx-question-mode-display-buffer-function #'pop-to-buffer-same-window)



  (define-key evil-motion-state-map (kbd "C-f") 'forward-char)
  (define-key evil-motion-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-motion-state-map (kbd "C-b") 'backward-char)
  (define-key evil-motion-state-map (kbd "C-d") 'delete-char)

  (defun my/mvn-dependency-version-to-properties ()
    (interactive)
    (save-excursion
      (search-forward "<version>")
      (kill-region (point) (progn
                             (search-forward "</version>")
                             (backward-char 10)
                             (point)))
      (let ((version (car kill-ring-yank-pointer)))
        (search-backward "<dependency>")
        (search-forward "<artifactId>")
        (kill-ring-save (point) (progn
                                  (search-forward "</artifactId>")
                                  (backward-char 13)
                                  (point)))
        (let ((group-id (car kill-ring-yank-pointer)))
          (search-backward "<dependency>")
          (search-forward "<version>")
          (insert "${" group-id ".version}")
          (search-backward "</properties>")
          (search-backward ">")
          (forward-char 1)
          (insert "\n")
          (indent-for-tab-command)
          (insert "<" group-id ".version>" version "</" group-id ".version>" )))))

  (defun my/mvn-inline-property ()
    "Inline mvn property."
    (interactive)
    (save-excursion
      (search-forward "<")
      (kill-region (point (progn
                            (search-forward ">"
                                            nil nil nil)
                            (backward-char 1)
                            (point))))
      (let ((property-name (car kill-ring-yank-pointer)))
        (forward-char 1)
        (kill-region (point) (progn
                               (search-forward "<"
                                               nil nil nil)
                               (backward-char 1)
                               (point)))

        (let ((property-value (car kill-ring-yank-pointer)))
          (beginning-of-line)
          (kill-line)
          (kill-line)
          (replace-match (concat "${" property-name "}") property-value)))))

  (defun my/mvn-sort-properties ()
    "Sort maven properties."
    (interactive "p")
    (save-excursion
      (beginning-of-buffer)
      (search-forward "<properties>")
      (set-mark-command (point))
      (search-forward "</properties>")
                                        ;(flush-lines "^\\s-*$"  (region-beginning) (region-end))
      (sort-lines nil (region-beginning) (region-end))))

  (defun my/other-window (arg)
    "Select ARGth window or switch buffer if there is only one window."
    (interactive "p")
    (let ((old-window  (selected-window)))
      (other-window arg)
      (when (equal old-window (selected-window))
        (other-frame arg))))

  ;; Auto refresh buffers
  (global-auto-revert-mode 1)

  ;; elfeed configuration
  (require 'elfeed)
  (setq elfeed-feeds
        '("http://sachachua.com/blog/feed/"
          "http://feeds.feedburner.com/cyclingnews/news?format=xml"))


  (require 'calendar)
  (define-key calendar-mode-map (kbd "<f2>") #'exco-calendar-show-day)

  (setq browse-url-browser-function 'my/browse-url)

  (defun my/browse-url (url new-window)
    "Browse url in the associated app.
URL - the url to browse.
new-window - whether to open in new window."
    (if (or (s-contains? "stackoverflow.com" url)
            (s-contains? "stackexchange.com" url))
        (sx-open-link url)
      (eww-follow-link)))

  (defun my/eww-follow-link (&optional external mouse-event)
    "Browse the URL under point.
If EXTERNAL is single prefix, browse the URL using `shr-external-browser'.
If EXTERNAL is double prefix, browse in new buffer."
    (interactive (list current-prefix-arg last-nonmenu-event))
    (mouse-set-point mouse-event)
    (let ((url (get-text-property (point) 'shr-url)))
      (cond
       ((not url)
        (message "No link under point"))
       ((string-match "^mailto:" url)
        (browse-url-mail url))
       ((and (consp external) (<= (car external) 4))
        (funcall shr-external-browser url))
       ;; This is a #target url in the same page as the current one.
       ((and (url-target (url-generic-parse-url url))
             (eww-same-page-p url (plist-get eww-data :url)))
        (let ((dom (plist-get eww-data :dom)))
          (eww-save-history)
          (eww-display-html 'utf-8 url dom nil (current-buffer))))
       ((string-prefix-p "http://www.google.bg/url?q=" url)
        (message "The url is url redirect.")
        (let* ((url-stripped-1 (s-replace "http://www.google.bg/url?q=" "" url))
               (url-stripped (s-left (s-index-of "&" url-stripped-1) url-stripped-1)))
          (message "Stripped google url: loading %s" url-stripped)
          (my/browse-url url-stripped external)))
       (t
        (my/browse-url url external)))))

  (setq large-file-warning-threshold nil)

  (defun my/emms-start ()
    "Start emms."
    (interactive)
    (emms-default-players)
    (emms-add-directory-tree "~/Music")
    (emms-toggle-random-playlist)
    (evil-evilified-state))

  ;; global leader key configuration
  (spacemacs/set-leader-keys "bb" 'helm-buffers-list)
  (spacemacs/set-leader-keys "cb" 'my/switch-to-compilation-buffer)
  (spacemacs/set-leader-keys "d" 'evil-operator-duplicate)
  (spacemacs/set-leader-keys "op" 'my/evil-replace-with-kill-ring)
  (spacemacs/set-leader-keys "ga" 'my/magit-stage-modified)
  (spacemacs/set-leader-keys "gc" 'magit-commit)
  (spacemacs/set-leader-keys "gwc" 'magit-wip-commit)
  (spacemacs/set-leader-keys "gwl" 'magit-wip-log)
  (spacemacs/set-leader-keys "od" 'my/duplicate-2)
  (spacemacs/set-leader-keys "of" 'my/switch-full-screen)
  (spacemacs/set-leader-keys "or" 'recentf-open-most-recent-file)
  (spacemacs/set-leader-keys "o=" 'my/format-defun)
  (spacemacs/set-leader-keys "ot" 'projectile-find-test-file)
  (spacemacs/set-leader-keys "ghn" 'diff-hl-next-hunk)
  (spacemacs/set-leader-keys "jr" 'jump-to-register)
  (spacemacs/set-leader-keys "ghk" 'diff-hl-revert-hunk)
  (spacemacs/set-leader-keys "ghv" 'diff-hl-mark-hunk)
  (spacemacs/set-leader-keys "ghp" 'diff-hl-previous-hunk)
  (spacemacs/set-leader-keys "pT" 'neotree-find-project-root)
  (spacemacs/set-leader-keys "so" 'helm-do-grep-ag)
  (spacemacs/set-leader-keys "xts" 'transpose-sexps)
  (spacemacs/set-leader-keys "gd" 'magit-diff-buffer-file)
  (spacemacs/set-leader-keys "pp" 'my/projectile-switch-project-dired)
  (spacemacs/set-leader-keys "pt" 'projectile-test-project)
  (spacemacs/set-leader-keys "ar" 'mu4e-alert-view-unread-mails)
  (spacemacs/set-leader-keys "ai" 'mu4e-alert-view-unread-mails)
  (spacemacs/set-leader-keys "os" 'my/store-the-default-buffer)
  (spacemacs/set-leader-keys "<SPC>" 'helm-buffers-list)
  (spacemacs/set-leader-keys "oo" 'my/go-to-the-default-buffer)
  (spacemacs/set-leader-keys "sR" 'my/helm-ag-recentf)
  (spacemacs/set-leader-keys "bl" 'my/list-repls)
  (spacemacs/set-leader-keys "mm" (lambda () (interactive)
                                    (mu4e~headers-jump-to-maildir "/Inbox")))


  (spacemacs/toggle-evil-visual-mark-mode-off)
  (spacemacs/toggle-mode-line-on)

  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

  (defun buffer-mode (buffer-or-string)
    "Returns the major mode associated with a buffer."
    (with-current-buffer buffer-or-string
      major-mode))


  (defun my/find-symbol-at-point ()
    "Find the function, face, or variable definition for the symbol at point
in the other window."
    (interactive)
    (let ((symb (symbol-at-point)))
      (cond
       ((and (or (functionp symb)
                 (fboundp symb))
             (find-definition-noselect symb nil))
        (find-function symb))
       ((and (facep symb) (find-definition-noselect symb 'defface))
        (find-face-definition symb))
       ((and (boundp symb) (find-definition-noselect symb 'defvar))
        (find-variable-other-window symb))
       (t (message "No symbol at point")))))

  (evil-define-operator evil-cp-change (beg end type register yank-handler delete-func)
    "Call `evil-change' while keeping parentheses balanced."
    :move-point t
    (interactive "<R><x><y>")
    (if (or (= beg end)
            (evil-cp--override)
            (and (eq type 'block) (evil-cp--balanced-block-p beg end))
            (and (sp-region-ok-p beg end) (not (eq type 'block))))
        (evil-change beg end type register yank-handler delete-func)
      (let ((delete-func (or delete-func #'evil-cp-delete))
            (nlines (1+ (- (line-number-at-pos end)
                           (line-number-at-pos beg))))
            (opoint (save-excursion
                      (goto-char beg)
                      (line-beginning-position))))
        (cond ((eq type 'line)
               (save-excursion
                 (evil-cp--delete-characters
                  (+ beg
                     (save-excursion
                       (beginning-of-line)
                       (sp-forward-whitespace t)))
                  (1- end)))
               (evil-cp-first-non-blank-non-opening)
               (indent-according-to-mode)
               (evil-insert 1))

              ((eq type 'block)
               (evil-cp-delete beg end type register yank-handler)
               (evil-insert 1 nlines))

              (t
               (funcall delete-func beg end type register yank-handler)
               (evil-insert 1))))))

  (setq helm-imenu-fuzzy-match nil)

  (require 'recentf)
  (recentf-auto-cleanup)

  (custom-set-variables
   '(helm-ag-command-option "-i"))

  (defun my/helm-ag-recentf ()
    "Search through the recent file."
    (interactive)
    (helm-do-ag "~/" recentf-list))

  (defun my/helm-ag-recentf-only-matches ()
    "Search through the recent file."
    (interactive)
    (let ((helm-ag-command-option "-i -l"))
      (helm-do-ag "~/" recentf-list)))

  (defun my/store-the-default-buffer ()
    "Stores the default buffer."
    (interactive)
    (setq my/default-buffer (or (buffer-file-name (current-buffer)) (buffer-name (current-buffer))))
    (message (s-concat "Stored the default buffer:"   (if (bufferp my/default-buffer)
                                                          (buffer-name my/default-buffer)
                                                        my/default-buffer))))

  (defun my/go-to-the-default-buffer ()
    "Goes to the default buffer."
    (interactive)
    (if (file-exists-p my/default-buffer)
        (find-file my/default-buffer)
      (switch-to-buffer my/default-buffer)))

  (global-set-key [(control down-mouse-1)]
                  (lambda (click)
                    (interactive "e")
                    (mouse-minibuffer-check click)
                    (let* ((window (posn-window (event-start click)))
                           (buf (window-buffer window)))
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char (posn-point (event-start click)))
                          (my/find-symbol-at-point))))))

  (setq magit-save-repository-buffers 'dontask)

  (setq-default default-input-method 'bulgarian-phonetic)

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; bind key
  (bind-key "C-h" 'backward-delete-char)
  (bind-key "M-/" 'hippie-expand)
  (bind-key "M-j" 'evil-join)
  (bind-key "C-x k" 'kill-current-buffer)
  (bind-key "C-M-h" 'backward-kill-word)
  (bind-key "C-j" 'newline-and-indent)

  (evil-define-text-object my/function-text-object (count)
    "Function text object"
    (interactive)
    (save-mark-and-excursion
     (mark-defun)
     (let ((m (mark)))
       (if (looking-back "*/\n")
           (progn
             (previous-line)
             (list m (first (sp-get-comment-bounds))))
         (list m (point))))))

  (evil-define-text-object my/statement-text-object (count)
    "Statement text object."
    (interactive)
    (save-mark-and-excursion
     (call-interactively 'c-beginning-of-statement)
     (let ((point-start (point)))
       (c-end-of-statement count)
       (list point-start (point)))))

  (define-key evil-inner-text-objects-map "m" 'my/function-text-object)
  (define-key evil-outer-text-objects-map "m" 'my/function-text-object)
  (define-key evil-outer-text-objects-map "e" 'my/statement-text-object)

  (load-file "~/.remote-config/config/my-mu4e.el")
  (load-file "~/.remote-config/config/my-pidgin.el")
  (load-file "~/.remote-config/config/my-cider.el")
  (load-file "~/.remote-config/config/my-java.el")
  (load-file "~/.remote-config/config/my-dired.el")
  (load-file "~/.remote-config/config/my-snippets.el")
  (customize-variable 'helm-exit-idle-delay)
  ;;
  (require 'helm)
  (setq-default helm-display-function 'helm-default-display-buffer)

  ;; cucumber
  (require 'feature-mode)

  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

  (setq feature-step-search-path "features/steps/*steps.el")

  (set-frame-name "emacs")

  (evil-define-key 'normal evil-mc-key-map (kbd "C-p") 'evil-paste-pop)
  (evil-define-key 'visual evil-mc-key-map (kbd "C-p") 'evil-paste-pop)

  (setq w3m-default-display-inline-images t)
  ;; w3m mode
  (setq w3m-home-page "https://www.google.com")
  ;; W3M Home Page
  (setq w3m-default-display-inline-images t)
  (setq w3m-default-toggle-inline-images t)
  ;; W3M default display images
  (setq w3m-command-arguments '("-cookie" "-F"))
  (setq w3m-use-cookies t)
  ;; W3M use cookies
  (setq browse-url-browser-function 'w3m-browse-url)
  ;; Browse url function use w3m
  (setq w3m-view-this-url-new-session-in-background t)
  ;; W3M view url new session in background

  (require 'flash-region)
  (defun my/flash-region (beg end &optional register yank-handler)
    (flash-region beg end eval-sexp-fu-flash-face 0.1))

  (add-function :before (symbol-function 'evil-yank-characters) #'my/flash-region)
  (add-function :before (symbol-function 'evil-yank-lines) #'my/flash-region)
  (add-function :before (symbol-function 'evil-yank-rectangle) #'my/flash-region))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eww-search-prefix "https://www.google.com/search?q=")
 '(package-selected-packages
   (quote
    (gradle-mode ensime sbt-mode scala-mode company-emacs-eclim eclim ecukes ansi espuds commander symon string-inflection sayid realgud test-simple loc-changes load-relative password-generator org-brain impatient-mode helm-purpose window-purpose evil-org evil-lion editorconfig dante cmake-ide levenshtein browse-at-remote zonokai-theme yapfify yaml-mode yahoo-weather xterm-color ws-butler winum which-key web-mode web-beautify w3m volatile-highlights vi-tilde-fringe uuidgen use-package unfill toc-org tide tagedit sx sr-speedbar sql-indent spaceline smeargle slim-mode skype shell-pop scss-mode sass-mode restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racket-mode pyvenv pytest pyenv-mode py-isort pug-mode powershell pip-requirements persp-mode persistent-scratch pcre2el paradox origami orgit org-projectile org-present org-pomodoro org-jira org-download org-bullets open-junk-file neotree mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode meghanada markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode key-chord json-mode js2-refactor js-doc java-snippets intero info+ indent-guide imenu-list ibuffer-projectile hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy forecast flyspell-correct-helm flycheck-pos-tip flycheck-haskell flycheck-clojure flx-ido fill-column-indicator fasd fancy-battery eyebrowse expand-region exec-path-from-shell excorporate eww-lnum evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-text-object-python evil-surround evil-smartparens evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emms emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies easy-kill dumb-jump disaster dired-subtree dired-efap dired+ diff-hl define-word cython-mode cypher-mode csv-mode company-web company-tern company-statistics company-ghci company-ghc company-cabal company-c-headers company-anaconda command-log-mode column-enforce-mode color-identifiers-mode coffee-mode cmm-mode cmake-mode clojure-snippets clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu autopair auto-yasnippet auto-highlight-symbol auto-dictionary auto-complete-nxml auto-compile all-the-icons-dired aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.))
