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
     html
     typescript
     mu4e
     javascript
     python
     helm auto-completion better-defaults emacs-lisp git markdown
     org spell-checking syntax-checking ibuffer clojure
     jabber
                                        ;no-dots command-log
     my-layer elfeed)

   dotspacemacs-additional-packages
   '(java-snippets ac-cider evil
                   sx crux elfeed jabber midje-mode flycheck-pos-tip
                   auto-complete-nxml sr-speedbar meghanada elpy dired+
                   dired-explorer dired-efap dired+ emms god-mode zenburn-theme
                   easy-kill ace-link java-snippets ac-cider evil notify sx crux
                   elfeed jabber midje-mode flycheck-pos-tip auto-complete-nxml
                   sr-speedbar meghanada elpy dired+ dired-explorer dired-efap
                   dired+ emms god-mode zenburn-theme easy-kill ac-cider eww
                   flycheck-pos-tip jabber persistent-scratch crux elfeed
                   midje-mode auto-complete-nxml sx sr-speedbar meghanada elpy
                   helm-dash dired+ dired-explorer dired-efap magit dired-subtree
                   emms org-jira-mode god-mode zenburn-theme easy-kill)

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
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Consolas"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-l"
   dotspacemacs-remap-Y-to-y$ nil
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
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
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
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used  for now. (default nil)
   dotspacemacs-default-package-repository) nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup `trailing
   )


(defun dotspacemacs/user-init ()
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")))
  )
(defun dotspacemacs/user-config ()
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

  ;; contrast configuration
  (require 'shr-color)
  (setq shr-color-visible-distance-min 60)
  (setq shr-color-visible-luminance-min 80)

  (smartparens-global-strict-mode t)
  (sp-use-paredit-bindings))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/savefile/bookmarks")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(excorporate-configuration
   (quote
    ("ivan.yonchovski@tick42.com" . "https://pod51036.outlook.com/ews/Exchange.asmx")))
 '(global-auto-highlight-symbol-mode t)
 '(global-command-log-mode t)
 '(jabber-alert-muc-hooks nil)
 '(jabber-alert-presence-hooks nil)
 '(jabber-mode-line-compact t)
 '(jabber-mode-line-mode nil)
 '(mu4e-hide-index-messages t)
 '(package-selected-packages
   (quote
    (notify enotify excorporate cypher-mode org-jira web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data web-mode-edit-element multi-web-mode mu4e-maildirs-extension mu4e-alert ht sx sr-speedbar persistent-scratch midje-mode meghanada java-snippets jabber fsm helm-dash god-mode flycheck-clojure emms elpy find-file-in-project ivy easy-kill dired-subtree dired-hacks-utils dired-explorer dired-efap dired+ crux auto-complete-nxml ac-cider zenburn-theme yapfify ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tide spacemacs-theme spaceline smeargle restart-emacs rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort pip-requirements persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file neotree mwim move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu elisp-slime-nav elfeed-web elfeed-org elfeed-goodies dumb-jump define-word cython-mode company-tern company-statistics company-anaconda command-log-mode column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" "target" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "target")))
 '(select-enable-clipboard t)
 '(send-mail-function (quote smtpmail-send-it))
 '(speedbar-show-unknown-files t)
 '(which-key-idle-delay 1.0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
