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
   '(vimscript
     javascript
     vinegar
     (treemacs :variables
               treemacs-use-filewatch-mode t)
     (java :variables
           java-backend 'lsp
           lsp-ui-doc-enable nil
           lsp-ui-sideline-enable t)
     spacemacs
     spacemacs-base
     spacemacs-evil
     spacemacs-bootstrap
     version-control
     csv
     windows-scripts
     yaml
     html
     typescript
     mu4e
     lsp
     fasd
     colors
     (shell :variables shell-default-shell 'eshell)
     eww
     (python :variables python-backend 'lsp)
     helm
     auto-completion
     spacemacs-layouts
     evil-commentary
     emacs-lisp
     git
     markdown
     org
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     ibuffer
     clojure
     command-log
     (elfeed :variables
             elfeed-feeds '("http://sachachua.com/blog/feed/"
                            "http://nullprogram.com/feed/"
                            "https://www.dnevnik.bg/author/rss"))
     bm
     spacemacs-purpose
     evil-snipe
     erc
     github)
   dotspacemacs-additional-packages
   '(java-snippets
     flash-region
     tabbar
     (treemacs :location "/home/kyoncho/Sources/lsp/treemacs/src/elisp/")
     lsp-ui
     ecukes
     feature-mode
     emms
     evil-smartparens
     excorporate
     cypher-mode
     org-jira
     sx
     persistent-scratch
     diff-hl
     helm-dash
     dired-subtree
     flycheck-clojure
     all-the-icons-dired
     helm-bm
     w3m
     emr
     dired-collapse
     dired-ranger
     dired-filter
     pretty-mode
     (shen-elisp
      :location (recipe :repo "deech/shen-elisp"
                        :fetcher github
                        :files ("shen*.el"))
      :upgrade 't)

     (shen-mode
      :location (recipe :repo "eschulte/shen-mode"
                        :fetcher github
                        :files ("*.el"))
      :upgrade 't)
     inf-clojure
     (targets :location
              (recipe :repo "noctuid/targets.el" :fetcher github :files ("*.el")))
     tree-mode
     undercover
     helm-cider
     bui
     package-lint
     md4rd
     autopair)
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
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-startup-lists '((recents . 5) (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-default-font '("Source Code Pro Medium"
                               :size 14
                               :weight normal
                               :weight normal
                               :width normal
                               :powerline-scale 0.7)
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
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 1.0
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 100
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis 'all
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()
  "User init."
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(cljr-helm . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)

  (setq-default custom-file "~/.remote-config/config/.custom.el"))

(defun dotspacemacs/user-config ()
  "User config."
  (-map 'load-file (directory-files "/home/kyoncho/Sources/lsp/treemacs/src/elisp/" t ".*el"))
  (use-package helm-projectile
    :defer t
    :config
    (setq helm-projectile-fuzzy-match nil))

  (use-package evil-cleverparens
    :defer t
    :init
    (spacemacs|diminish evil-cleverparens-mode))

  (use-package ggtags
    :defer t
    :init (spacemacs|diminish ggtags-mode "GT" ""))

  (use-package eww
    :requires (evil-evilified-state)
    :defer t
    :hook (eww-mode-hook . #'evil-evilified-state)
    :bind (:map eww-mode-map
                ("f" . 'ace-link-eww)
                ("g" . 'eww)
                ("p" . 'eww-back-url)
                ("n" . 'eww-forward-url)
                ("G" . 'eww-reload)
                ("h" . 'helm-eww-history)
                ("l" . 'helm-eww-links)))

  (setq helm-ff-guess-ffap-filenames t)

  ;; do not ask when deleting buffers
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  (smartparens-global-strict-mode t)

  (evil-visual-mark-mode t)

  (sp-use-paredit-bindings)

  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-automatic-symbol-highlight-on)


  (c-set-offset 'substatement-open 0)

  ;; needed for equkes
  (setenv "EMACS" (expand-file-name "~/.bin/emacs/bin/emacs26"))

  (use-package evil-smartparens
    :defer t
    :config
    (sp-pair "(" ")" :wrap "M-(")
    (sp-pair "{" "}" :wrap "M-{")
    (sp-pair "[" "]" :wrap "M-["))

  ;; evil-mc configuration
  (global-evil-mc-mode t)

  (evil-define-operator my/evil-operator-duplicate (beg end)
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
      (goto-char beg)
      (call-interactively 'evil-paste-before 1)))

  (setq kill-do-not-save-duplicates t
        make-backup-files nil)
  (global-subword-mode t)

  (custom-set-variables '(evil-want-C-i-jump t))
  (evil-set-command-property 'evil-mc-skip-and-goto-next-match :jump t)
  (evil-set-command-property 'find-file-at-point :jump t)
  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

  (defun my/switch-to-compilation-buffer (arg)
    "Switch to compilation buffer"
    (interactive "P")
    (switch-to-buffer "*compilation*"))

  (add-hook 'custom-mode-hook 'evil-evilified-state)

  (defun my/find-pom-file ()
    "Find file in upper dirs"
    (interactive)
    (if-let* (pf (expand-file-name
                  (concat (locate-dominating-file
                           (if (string= (file-name-nondirectory (buffer-file-name)) "pom.xml")
                               (file-name-directory
                                (directory-file-name (file-name-directory (buffer-file-name))))
                             (buffer-file-name))
                           "pom.xml")
                          "pom.xml")))
        (find-file pf)
      (message "Unable to find pom.xml")))
  (global-evil-surround-mode 1)

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

  (define-key evil-normal-state-map "\t" 'indent-for-tab-command)
  (define-key evil-normal-state-map "go" 'my/evil-replace-with-kill-ring)

  ;; projectile
  (use-package projectile
    :defer t
    :config
    (setq projectile-create-missing-test-files t
          projectile-globally-ignored-files (list "TAGS" ".lein-repl-history")
          projectile-globally-ignored-directories (list ".idea" ".ensime_cache" ".eunit" "target" ".git" ".hg" ".fslckout"
                                                        "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "target" ".cask")))



  (use-package evil-cleverparens
    :defer t
    :config
    (global-set-key [remap evil-cp-end-of-defun] 'my/goto-end-of-form))

  ;; Also auto refresh dired, but be quiet about it
  (require 'autorevert)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; emms configuration
  (use-package emms-setup
    :config (progn
              (emms-all)
              (emms-default-players)
              (emms-mode-line -1)))

  (use-package company
    :defer t
    :config
    (progn
      (define-key company-active-map (kbd "<escape>") 'company-abort)
      (setq-default company-auto-complete-chars '(?\) ?.))
      (setq-default company-auto-complete t)))

  (use-package magit
    :defer t
    :config
    (setq magit-diff-arguments '("--stat" "--no-ext-diff" "--ignore-all-space")
          magit-save-repository-buffers 'dontask
          magit-revision-show-gravatars nil
          magit-display-buffer-function 'magit-display-buffer-traditional)

    (defun my/magit-stage-modified ()
      "Stage all changes to files"
      (interactive)
      (magit-with-toplevel
        (magit-stage-1 "--all"))))

  (defun my/eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))

  (use-package evil
    :defer t
    :config
    (setq evil-move-beyond-eol t
          evil-cross-lines t))

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

  (display-time-mode t)

  ;; indent mode
  (spacemacs/toggle-indent-guide-globally-off)

  (setq indent-guide-inhibit-modes '(tabulated-list-mode
                                     special-mode dired-mode java-mode emacs-lisp-mode web-mode
                                     eww-mode eshell-mode Custom-mode))

  ;; always follow symlinks
  (setq vc-follow-symlinks t)

  (use-package vc-dispatcher
    :defer t
    :config
    (setq vc-suppress-confirm nil))

  ;; cleverparens configuration
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode-enable)

  (add-hook 'yas-before-expand-snippet-hook (lambda () (autopair-mode 1)))
  (add-hook 'yas-after-exit-snippet-hook (lambda () (autopair-mode -1)))

  (defun my/two-monitors ()
    "Set frame size to cover 2 monitors"
    (interactive)
    (setq frame-resize-pixelwise t)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame) (* 2 1920) 1080 t))

  (use-package sx-interaction
    :defer t
    :config
    (setq sx-question-mode-display-buffer-function #'pop-to-buffer-same-window))

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

  (global-auto-revert-mode 1)

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

  (spacemacs/set-leader-keys "ae" 'emms)
  (spacemacs/set-leader-keys "it" 'bm-toggle)
  (spacemacs/set-leader-keys "in" 'bm-next)
  (spacemacs/set-leader-keys "ii" 'helm-bm)
  (spacemacs/set-leader-keys "iN" 'bm-previous)
  (spacemacs/set-leader-keys "cb" 'my/switch-to-compilation-buffer)
  (spacemacs/set-leader-keys "dd" 'my/evil-operator-duplicate)
  (spacemacs/set-leader-keys "JPM" 'my/send-to-jpm)
  (spacemacs/set-leader-keys "op" 'my/evil-replace-with-kill-ring)
  (spacemacs/set-leader-keys "ga" 'my/magit-stage-modified)
  (spacemacs/set-leader-keys "gC" 'magit-commit-extend)
  (spacemacs/set-leader-keys "gc" 'magit-commit)
  (spacemacs/set-leader-keys "wT" 'my/toggle-window-split)
  (spacemacs/set-leader-keys "od" 'my/duplicate-2)
  (spacemacs/set-leader-keys "of" 'my/helm-find-file-in-directory)
  (spacemacs/set-leader-keys "or" 'recentf-open-most-recent-file)
  (spacemacs/set-leader-keys "o=" 'my/format-defun)
  (spacemacs/set-leader-keys "ot" 'projectile-find-test-file)
  (spacemacs/set-leader-keys "ghk" 'diff-hl-revert-hunk)
  (spacemacs/set-leader-keys "xts" 'transpose-sexps)
  (spacemacs/set-leader-keys "gd" 'magit-diff-buffer-file)
  (spacemacs/set-leader-keys "ag" 'browse-url-chrome)
  (spacemacs/set-leader-keys "pp" 'my/projectile-switch-project-dired)
  (spacemacs/set-leader-keys "TE" 'emacs-lisp-mode)
  (spacemacs/set-leader-keys "os" 'my/store-the-default-buffer)
  (spacemacs/set-leader-keys "oo" 'my/go-to-the-default-buffer)
  (spacemacs/set-leader-keys "sm" 'helm-mu)
  (spacemacs/set-leader-keys "sR" 'my/helm-ag-recentf)
  (spacemacs/set-leader-keys "mm" (lambda () (interactive)
                                    (mu4e~headers-jump-to-maildir "/Inbox")))

  ;; shell configuration
  (defun my/shell-pop-no-cd (arg)
    (interactive "P")
    (let ((shell-pop-autocd-to-working-dir nil))
      (shell-pop arg)))

  (spacemacs/set-leader-keys "\"" 'my/shell-pop-no-cd)

  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

  (defun buffer-mode (buffer-or-string)
    "Returns the major mode associated with a buffer."
    (with-current-buffer buffer-or-string
      major-mode))

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

  (use-package recentf
    :defer t
    :config
    (recentf-auto-cleanup))

  (custom-set-variables
   '(helm-ag-command-option "-i"))



  (setq-default default-input-method 'bulgarian-phonetic)

  ;; bind key
  (require 'cc-mode)


  (bind-key "C-j" 'newline-and-indent)

  (use-package evil
    :defer t
    :config

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

    (setq evil-move-beyond-eol t
          evil-cross-lines t)

    (define-key evil-motion-state-map (kbd "C-f") 'forward-char)
    (define-key evil-motion-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-motion-state-map (kbd "C-b") 'backward-char)
    (define-key evil-motion-state-map (kbd "C-d") 'delete-char)

    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

    (define-key evil-inner-text-objects-map "m" 'my/function-text-object)
    (define-key evil-outer-text-objects-map "m" 'my/function-text-object)
    (define-key evil-outer-text-objects-map "e" 'my/statement-text-object))

  (load-file "~/.remote-config/config/my-mu4e.el")
  (load-file "~/.remote-config/config/my-emacs-lisp.el")
  (load-file "~/.remote-config/config/my-tabbar.el")
  (load-file "~/.remote-config/config/my-cider.el")
  (load-file "~/.remote-config/config/my-lsp.el")
  (load-file "~/.remote-config/config/my-dired.el")
  (load-file "~/.remote-config/config/my-snippets.el")
  (load-file "~/.remote-config/config/local.el")

  (defun my/capitalize-first-char (&optional string)
    "Capitalize only the first character of the input STRING."
    (when (and string (> (length string) 0))
      (let ((first-char (substring string nil 1))
            (rest-str   (substring string 1)))
        (concat (capitalize first-char) rest-str))))

  (use-package imenu
    :defer t
    :config
    (setq imenu-create-index-function 'imenu-default-create-index-function)
    (setq imenu-auto-rescan t))

  (use-package helm
    :defer t
    :config
    (setq-default helm-display-function 'helm-default-display-buffer
                  helm-exit-idle-delay 0)

    (defun my/helm-ag-recentf ()
      "Search through the recent file."
      (interactive)
      (recentf-cleanup)
      (helm-do-ag "~/" (-filter
                        'file-exists-p
                        (-remove
                         (lambda (s)
                           (or (s-starts-with-p "/ssh:" s)
                               (s-starts-with-p "/sudo:" s)))
                         recentf-list))))

    (defun my/helm-ag-recentf-only-matches ()
      "Search through the recent file."
      (interactive)
      (let ((helm-ag-command-option "-i -l"))
        (helm-do-ag "~/" recentf-list)))

    (setq helm-imenu-fuzzy-match nil))

  (use-package feature-mode
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

  (setq feature-step-search-path "features/steps/*steps.el")

  (set-frame-name "emacs")

  (evil-define-key 'normal evil-mc-key-map (kbd "C-p") 'evil-paste-pop)
  (evil-define-key 'visual evil-mc-key-map (kbd "C-p") 'evil-paste-pop)

  (require 'w3m)
  (setq w3m-home-page "https://www.google.com")
  (setq w3m-default-display-inline-images t)
  (setq w3m-default-toggle-inline-images t)
  (setq w3m-command-arguments '("-cookie" "-F"))
  (setq w3m-use-cookies t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-view-this-url-new-session-in-background t)

  (require 'flash-region)
  (defun my/flash-region (beg end &optional register yank-handler)
    (flash-region beg end nil 0.1))

  (add-function :before (symbol-function 'evil-yank-characters) #'my/flash-region)
  (add-function :before (symbol-function 'evil-yank-lines) #'my/flash-region)
  (add-function :before (symbol-function 'evil-yank-rectangle) #'my/flash-region)

  (defun my/helm-find-file-in-directory ()
    "Find file in current directory"
    (interactive)
    (let ((projectile-cached-project-root default-directory))
      (projectile-find-file)))

  (setenv "PATH" (concat (getenv "PATH") ":~/.bin"))

  (require 'browse-url)
  (setq browse-url-browser-function 'browse-url-chrome
        browse-url-generic-program "google-chrome")

  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (global-set-key [remap eww-follow-link] 'my/eww-follow-link)

  (setq url-user-agent (concat
                        "User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) "
                        "AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7\n")
        w3m-user-agent nil
        w3m-use-cookies t)

  (use-package persistent-scratch
    :init (persistent-scratch-setup-default))

  (setq xref-prompt-for-identifier nil)
  (global-subword-mode t)
  (which-function-mode t)

  (condition-case nil
      (load-file "~/.remote-config/config/my-pidgin.el")
    (error
     (message "Error loading pidgin...")))

  (add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

  (spaceline-toggle-buffer-id-off)

  (add-to-list 'evil-lisp-safe-structural-editing-modes 'java-mode)

  (setq helm-display-buffer-default-height 15)
  (setq helm-buffer-max-length 60)

  (defun my/toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  ;; jira configuration
  (use-package org-jira
    :defer t
    :init
    (require 'jiralib)
    (setq jiralib-url "https://jira.tick42.com"
          jiralib-user-login-name "iyonchovski"))

  (use-package org
    :defer t
    :config
    (require 'org-agenda)
    (setq org-agenda-files (directory-files "~/.org-jira" t "^[[:alpha:])_]+.org"))
    (require 'org-jira)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jp" 'org-jira-progress-issue)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jg" 'org-jira-get-issue)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "ji" 'org-jira-update-issue)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jG" 'org-jira-get-issues)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jr" 'org-jira-refresh-issue)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jR" 'org-jira-refresh-issues-in-buffer)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jc" 'org-jira-update-comment)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jd" 'org-jira-download-attachment)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jt" 'org-jira-todo-to-jira)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jb" 'org-jira-browse-issue)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "jl" 'org-jira-update-worklogs-from-org-clocks)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "=" 'org-timestamp-up)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "-" 'org-timestamp-down)
    (remove-hook 'org-mode-hook 'spacemacs/delay-emoji-cheat-sheet-hook))

  (spacemacs|use-package-add-hook which-key
    :post-init (setq which-key-idle-delay 1.0))

  (setq excorporate-configuration
        '("ivan.yonchovski@tick42.com" . "https://pod51036.outlook.com/ews/Exchange.asmx"))

  (defun hide-ctrl-M ()
    "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

  (use-package treemacs
    :defer t
    :init
    (progn
      (defun my/treemacs-ignored-predicates (file _)
        "Ignored predicates."
        (s-matches? (rx bol
                        (or ".git" ".project" ".settings" ".classpath" ".meghanada" ".idea" ".vscode" ".gitignore" ".cask")
                        eol)
                    file))

      (setq treemacs-ignored-file-predicates
            '(treemacs--std-ignore-file-predicate my/treemacs-ignored-predicates)
            treemacs-collapse-dirs 3)

      (require 'treemacs-follow-mode)
      (treemacs-follow-mode t)
      (require 'treemacs-filewatch-mode)
      (treemacs-filewatch-mode t)))

  (helm-flx-mode -1)
  (require 'nameless)
  (add-to-list 'nameless-global-aliases (cons "L" "lsp"))
  (add-to-list 'nameless-global-aliases (cons "D" "dap"))
  (add-to-list 'nameless-global-aliases (cons "J" "dap-java"))
  (add-to-list 'nameless-global-aliases (cons "P" "dap-python"))

  (evil-set-command-property 'lsp-goto-type-definition :jump t)
  (evil-set-command-property 'lsp-goto-implementation :jump t)
  (add-hook 'projectile-after-switch-project-hook 'treemacs-projectile)

  (defun my/evil-iedit-state/iedit-mode (&optional arg)
    "Start `iedit-mode'."
    (interactive "P")
    (if (fboundp 'ahs-clear) (ahs-clear))
    (iedit-mode (or arg 0))
    (evil-iedit-state))

  (spacemacs/set-leader-keys "sE" 'my/evil-iedit-state/iedit-mode)
  (spacemacs/set-leader-keys "se" 'evil-iedit-state/iedit-mode)

  (define-key evil-ex-map "C-b" 'backward-char)
  (define-key evil-ex-map "C-f" 'forward-char)

  (setq recentf-exclude
        '("/git-rebase-todo\\'" "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "COMMIT_EDITMSG\\'" ))

  (use-package eshell
    :defer t
    :config
    (add-to-list 'eshell-visual-commands "htop" "finch")
    (defun my/eshell-kill-output ()
      "Kill all output from interpreter since last input.
Does not delete the prompt."
      (interactive)
      (save-excursion
        (goto-char (eshell-beginning-of-output))
        (insert "*** output flushed ***\n")
        (kill-region (point) (eshell-end-of-output))))

    (evil-define-key 'normal eshell-mode-map  (kbd "C-r") 'helm-eshell-history)
    (evil-define-key 'visual eshell-mode-map  (kbd "C-r") 'helm-eshell-history)
    (evil-define-key 'insert eshell-mode-map  (kbd "C-r") 'helm-eshell-history)

    (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "k" 'my/eshell-kill-output)
    (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "p" 'helm-eshell-prompts)
    (spacemacs/set-leader-keys-for-major-mode 'eshell-mode "P" 'helm-eshell-prompts-all))
  (targets-setup t :around-key "R"))
