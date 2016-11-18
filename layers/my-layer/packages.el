;;; packages.el --- my-layer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ivan <kyoncho@myoncho>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-layer-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-layer/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-layer/pre-init-PACKAGE' and/or
;;   `my-layer/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-layer-packages
  '()
  "The list of Lisp packages required by the my-layer layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;;; packages.el ends here
;;; .emacs --
;;; Commentary:
;;; my YY Emacs configuration
;;; Code:

;; Check if we're on Emacs 24.4 or newer, if so, use the pinned package feature

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'flycheck-pos-tip)
(require 'auto-complete-nxml)
(smartparens-mode -1)

;; modes
(show-paren-mode 1)
(delete-selection-mode t)
(setq indent-tabs-mode nil)
(global-auto-highlight-symbol-mode t)
(line-number-mode 1)


;; magit configuration
(require 'magit)
(setq git-commit-summary-max-length 999)

(setq kill-do-not-save-duplicates t)

;; disable backup files
(setq make-backup-files nil)

(defun indent-buffer ()
  "Indent buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally))
 '(send-mail-function (quote smtpmail-send-it))
 '(speedbar-show-unknown-files t)
 '(x-select-enable-clipboard t))



;; dired configuration
(require 'dired-x)
(require 'dired+)
(require 'dired-explorer)
(require 'dired-efap)

(define-key dired-mode-map [f2] 'dired-efap)
(define-key dired-mode-map (kbd "I") 'dired-subtree-toggle)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq delete-by-moving-to-trash t)
(setq dired-recursive-deletes 'always)
(setq dired-deletion-confirmer '(lambda (x) t))

(defun my/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let ((window (selected-window)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(require 'org)
(require 'org)
;; eval and replace
(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))

             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)

;; file -> mode configuration
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))

;; cider configuration
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'auto-complete-mode)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook #'aggressive-indent-mode)
(add-hook 'cider-mode-hook #'auto-highlight-symbol-mode)

(require 'cider )
(setq cider-test-show-report-on-success nil)
(setq cider-prompt-save-file-on-load 'always-save)
(setq cider-use-fringe-indicators t)

(require 'midje-mode)
(setq midje-comments ";;.;.")

(setq cider-auto-mode 't)

(eval-after-load 'cider-mode
  '(define-key cider-mode-map (kbd "C-c M-r") 'cider-restart))

;; java configuration
(require 'cc-vars)

(defun java-conf ()
  "Java configuration function."
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil)
  (c-set-offset 'inline-open '=))

(require 'meghanada)
(add-hook 'java-mode-hook #'paredit-mode)
(add-hook 'java-mode-hook #'java-conf)
;;(remove-hook 'java-mode-hook #'meghanada-mode)
(add-hook 'java-mode-hook #'aggressive-indent-mode)
(add-hook 'java-mode-hook #'yas-minor-mode)

(require 'cc-mode)

(require 'org)
(define-key java-mode-map (kbd "C-x C-j")
  'projectile-find-implementation-or-test-other-window)

(require 'paredit)
(define-key paredit-mode-map (kbd "C-M-u") 'er/expand-region)
(define-key java-mode-map (kbd "C-<f11>") 'meghanada-run-junit-recent)

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; prevent creating backup files
(setq make-backup-files nil)

(defun join-next-line ()
  "Joins the next line into the current one."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (delete-char 1)
    (delete-horizontal-space)))

(require 'minibuffer)
(defun set-auto-complete-as-completion-at-point-function ()
  "Autocompletion function."
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(defun un-camelcase-word-at-point ()
  "Un-camelcase the word at point.
replacing uppercase chars with the lowercase version preceded by an underscore.
The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore"
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

(require 'ac-cider)
(setq cider-lein-command "~/.bin/lein")
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))
(eval-after-load 'cider-mode
  '(define-key cider-mode-map (kbd "C-S-f") 'cider-format-buffer))

(setenv "PATH" (concat (getenv "PATH") ":~/.bin"))

(put 'set-goal-column 'disabled nil)

(require 'magit)
(require 'ediff-diff)

(setq ediff-diff-options "-w")

(defun my/clojure-mode-hook ()
  "Clojure mode hook."
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c m"))

(add-hook 'clojure-mode-hook #'my/clojure-mode-hook)

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

(fset 'yes-or-no-p 'y-or-n-p)

;; prevent prompt for killing emcacsclient buffer
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(setq inhibit-splash-screen t)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)
(define-key term-raw-map  (kbd "C-y") 'term-paste)

(defun fullscreen ()
  "Switch to full screen."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(require 'god-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(setq shift-selection-mode t)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; auto complete settings
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(setq sgml-basic-offset 4)

(add-hook 'nxml-mode-hook
          (lambda()
            (paredit-mode t)
            (web-mode t)
            (local-unset-key (kbd "C-M-u"))))

(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")


(set-face-attribute 'default nil :height 130)

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck)
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       #'aggressive-indent-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'aggressive-indent-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(dolist (mode '(clojure-mode clojurescript-mode cider-mode))
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
            ("\\(#\\){"                 ; sets
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "∈")
                       nil)))))))


;; shortcuts configuration




;; configuration for multiple cursors

(define-key global-map (kbd "C-c SPC") 'avy-goto-word-1)
(define-key global-map (kbd "C-c C-SPC") 'avy-pop-mark)


(setq exec-path (append exec-path '("/usr/bin")))

(require 'company)
(define-key company-active-map "\C-p" 'company-select-previous)
(define-key company-active-map "\C-n" 'company-select-next)
(define-key company-active-map "\C-j" 'company-complete-selection)

(define-key java-mode-map "\M-j" 'join-next-line)

(define-key ac-complete-mode-map "\C-n" 'ac-previous)
(define-key ac-complete-mode-map "\C-p" 'ac-next)

(define-key ac-complete-mode-map "C-М-)" 'paredit-forward-slurp-sexp)

(setq prelude-whitespace nil)

(global-unset-key (vector (list 'shift 'left)))
(global-unset-key (vector (list 'shift 'right)))
(global-unset-key (vector (list 'shift 'up)))
(global-unset-key (vector (list 'shift 'down)))

(add-hook 'midje-mode-hook
          (lambda ()
            (message "passed ")
            (define-key midje-mode-map (kbd "C-c p") nil)))

(provide '.emacs)
;;; .emacs ends here
;; org-mode configuration
(setq org-hide-leading-stars t)


(setq-default ibuffer-saved-filter-groups
              `(("Default"
                 ;; I create a group call Dired, which contains all buffer in dired-mode
                 ("Dired" (mode . dired-mode))
                 ("Temporary" (name . "\*.*\*")))))

(setq browse-url-generic-program (executable-find "conkeror"))
(setq browse-url-browser-function 'browse-url-generic)

;; always follow symlinks
(setq vc-follow-symlinks t)

;; use windows-1251
(modify-coding-system-alist 'file "\\.txt\\'" 'windows-1251)

(set-face-attribute 'region nil :background "#666")

(setq-default cursor-type '(bar . 2))
(setq default-frame-alist '((cursor-color . "white")))

(defun my/increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'ARG'.
ARG - the amount for increasing the value."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun my/mvn-dependency-version-to-properties (&optional arg)
  (interactive "p")
  (save-excursion
    (search-forward "<version>")
    (kill-region (point) (progn
                           (search-forward "</version>"
                                           nil nil arg)
                           (backward-char 10)
                           (point)))
    (let ((version (car kill-ring-yank-pointer)))
      (search-backward "<dependency>")
      (search-forward "<artifactId>")
      (kill-ring-save (point) (progn
                                (search-forward "</artifactId>"
                                                nil nil arg)
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
  (interactive "p")
  (save-excursion
    (search-forward "<")
    (kill-region (point) (progn
                           (search-forward ">"
                                           nil nil nil)
                           (backward-char 1)
                           (point)))
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

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(require 'projectile)

(defun my/projectile-open-pom ()
  "Open's pom file from the project."
  (interactive)
  (find-file (concat (projectile-project-root) "pom.xml")))

(defun my/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun my/find-user-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/" t))))
         (shell-init-file (cond
                           ((string= "zsh" shell) crux-shell-zsh-init-files)
                           ((string= "bash" shell) crux-shell-bash-init-files)
                           ((string= "tcsh" shell) crux-shell-tcsh-init-files)
                           ((string= "fish" shell) crux-shell-fish-init-files)
                           ((string-prefix-p "ksh" shell) crux-shell-ksh-init-files)
                           (t (error "Unknown shell"))))
         (candidates (cl-remove-if-not 'file-exists-p (mapcar 'substitute-in-file-name shell-init-file))))
    (if (> (length candidates) 1)
        (find-file (completing-read "Choose shell init file:" candidates))
      (find-file (car candidates)))))

(require 'vc-dispatcher)
(setq vc-suppress-confirm nil)

;; helm configuration
(require 'helm)
(require 'projectile)
(require 'cider-eval-sexp-fu)

(defun my/other-window (arg)
  "Select ARGth window or switch buffer if there is only one window."
  (interactive "p")
  (let ((old-window  (selected-window)))
    (other-window arg)
    (when (equal old-window (selected-window))
      (other-frame arg))))

(defun my/toggle-window-split ()
  "Toggle window split."
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

(defun my/rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (let ((i 1)
               (numWindows (count-windows)))
           (while  (< i numWindows)
             (let* (
                    (w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))
                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))
                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1  b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Auto refresh buffers
(global-auto-revert-mode 1)

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


(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; redefine emacs state to intercept the escape key like insert-state does:

(defun my/projectile-find-implementation ()
  "Open matching implementation or test file in other window."
  (interactive)
  (find-file (projectile-find-implementation-or-test (buffer-file-name))))

(defadvice evil-insert-state (around emacs-state-instead-of-insert-state activate)
  "Use emacs state for insert mode."
  (evil-emacs-state))

;; clipboard

;; unset the suspend frame command
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; global key configuration
(bind-key "C-c C-<" 'mc/mark-all-like-this)
(bind-key "M-p" 'move-text-up)
(bind-key "M-n" 'move-text-down)
(bind-key "<C-f8>" 'bm-toggle)
(bind-key "<f8>"   'bm-next)
(bind-key "<M-f8>" 'bm-previous)
(bind-key "M-i" 'helm-swoop)
(bind-key "M-I" 'helm-swoop-back-to-last-point)
(bind-key "C-c M-i" 'helm-multi-swoop)
(bind-key "C-x M-i" 'helm-multi-swoop-all)
(bind-key "C-x k" 'kill-current-buffer)
(bind-key "<f7>" 'go-to-terminal-window)
(bind-key "<f11>" 'fullscreen)
(bind-key "M-j" 'join-next-line)
(bind-key "C-x B" 'ibuffer)
(bind-key "C-x C-r" 'helm-recentf)
(bind-key "M-/" 'hippie-expand)
(bind-key "C-l" 'other-window)
(bind-key "C-M-u" 'er/expand-region)
(bind-key "C->" 'mc/mark-next-like-this)
(bind-key "C-<" 'mc/mark-previous-like-this)
(bind-key "C-c C-<" 'mc/mark-all-like-this)
(bind-key "C-M-s" 'helm-swoop)
(bind-key "C-h" 'backward-delete-char)
(bind-key "C-M-h" 'backward-kill-word)
(bind-key "C-M-y" 'helm-show-kill-ring)
(bind-key "C-S-l" 'helm-projectile-ack)
(bind-key "C-S-c" 'comment-region)
(bind-key "C-v" 'ace-window)
(bind-key "C-c 9" 'buffer-menu)
(bind-key "C-x p" 'previous-buffer)
(bind-key "C-x n" 'next-buffer)
(bind-key "C-M-u" 'er/expand-region)
(bind-key "C-x 9" 'helm-locate)
(bind-key "C-<backspace>" 'subword-backward-kill)
(bind-key "C-x v" 'eval-buffer)
(bind-key "C-c C-c" 'eval-defun)
(bind-key "C-c h" 'helm-google-suggest)
(bind-key "C-x m" 'helm-M-x)
(bind-key "C-x b" 'helm-buffers-list)
(bind-key "C-x C-j" 'my/projectile-find-implementation)
(bind-key "C->" 'mc/mark-next-like-this)
(bind-key "C-<" 'mc/mark-previous-like-this)
(bind-key "M-<left>" 'back-button-global-backward)
(bind-key "M-<right>" 'back-button-global-forward)
(bind-key "C-c 1" 'switch-window)
(bind-key "<f6>" 'god-mode)
(bind-key "<f7>" 'sr-speedbar-toggle)
(bind-key "C-x d" 'dired)
(bind-key "C-v" 'change-inner)
(bind-key "M-v" 'copy-inner)
(bind-key "<f8>" 'emms)

(defun fg-emms-track-description (track)
  "Return a somewhat nice track description."
  (let ((artist (emms-track-get track 'info-artist))
        (year (emms-track-get track 'info-year))
        (album (emms-track-get track 'info-album))
        (tracknumber (emms-track-get track 'info-tracknumber))
        (title (emms-track-get track 'info-title)))
    (cond
     ((or artist title)
      (concat (if (> (length artist) 0) artist "Unknown artist") " - "
              (if (> (length year) 0) year "XXXX") " - "
              (if (> (length album) 0) album "Unknown album") " - "
              (if (> (length tracknumber) 0)
                  (format "%02d" (string-to-number tracknumber))
                "XX") " - "
                (if (> (length title) 0) title "Unknown title")))
     (t
      (emms-track-simple-description track)))))

(require 'emms)
(setq emms-track-description-function 'fg-emms-track-description)

(require 'crux)

(defun my/start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer buffer-name)))

(defun my/visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (my/start-or-switch-to (lambda ()
                           (ansi-term crux-shell (concat crux-term-buffer-name "-term")))
                         (format "*%s-term*" crux-term-buffer-name)))


(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap other-window] 'my/other-window)
(global-set-key [remap crux-find-user-init-file] 'my/find-user-init-file)
(global-set-key [remap crux-find-shell-init-file] 'my/find-user-shell-init-file)
(global-set-key [remap crux-visit-term-buffer] 'my/visit-term-buffer)

(bind-key "C-x 2" 'my/vsplit-last-buffer)
(bind-key "C-x 3" 'my/hsplit-last-buffer)
(bind-key "C-c w w" 'w3m)
(bind-key "C-c M-p" 'my/projectile-open-pom)

(require 'ivy)
(bind-key "C-c C-SPC" 'ivy-goto-word)

(require 'nxml-mode)
(bind-key "C-c M-e" 'my/mvn-dependency-version-to-properties nxml-mode-map)

(require 'dired-x)


(require 'auto-complete-nxml)
;; Keystroke to popup help about something at point.
(setq auto-complete-nxml-popup-help-key "C-:")
;; Keystroke to toggle on/off automatic completion.
(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)

;; python configuration

(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'python-mode-hook #'eldoc-mode)


(require 'evil)
(setq evil-default-state 'emacs)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)
;; (require 'mu4e-speedbar)

;; (defun my/mu4e-go-to-inbox ()
;;   "Go to inbox."
;;   (interactive)
;;   (mu4e-headers-search
;;    (format "maildir:\"%s\"" "/INBOX")))

;; (bind-key "C-c m" 'mu4e)


;; (setq mu4e-drafts-folder "/Drafts"
;;       mu4e-sent-folder   "/Sent Items"
;;       mu4e-trash-folder  "/Trash"
;;       mu4e-msg2pdf "/usr/bin/msg2pdf"
;;       mu4e-update-interval 200)

;; ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'sent)

;; (setq mu4e-maildir-shortcuts
;;       '(("/INBOX" . ?j)
;;         ("/Drafts" . ?d)
;;         ("/Trash" . ?t)
;;         ("/Sent Items" . ?s)
;;         ("/bamboo" . ?b)))

;; ;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
;; (setq
;;  user-mail-address "ivan.yonchovski@tick42.com"
;;  user-full-name  "Ivan Yonchovski"
;;  mu4e-compose-signature nil)

;; (require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       '(("smtp.office365.com" 587 "ivan.yonchovski@tick42.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.office365.com"
;;       smtpmail-smtp-service 587)

;; (setq message-kill-buffer-on-exit t)

;; (use-package mu4e-alert
;;   :ensure t
;;   :config
;;   (mu4e-alert-enable-notifications)
;;   (mu4e-alert-set-default-style 'libnotify)
;;   (setq mu4e-alert-interesting-mail-query
;;         (concat "maildir:/INBOX and flag:unread"))

;;   (alert-add-rule
;;    :category "mu4e-alert"
;;    :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
;;    :continue )

;;   ;; display stuff on modeline as well as notify
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))


;; elfeed configuration
(require 'elfeed)
(setq elfeed-feeds
      '("http://sachachua.com/blog/feed/"
        "http://feeds.feedburner.com/cyclingnews/news?format=xml"))

(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-mode-line -1)

(setq browse-url-browser-function 'eww-browse-url
      browse-url-generic-program "chromium-browser")

;; jabber configuration
(require 'jabber)
(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      jabber-connection-type  'ssl
      jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 1000
      jabber-backlog-days 300
      starttls-extra-arguments '("--starttls"))

(jabber-mode-line-mode t)

(setq jabber-invalid-certificate-servers '("PIRINSOFT"))
(setq jabber-account-list
      '(("iyonchovski@PIRINSOFT/work"
         (:network-server . "jabber.pirinsoft.bg")
         (:port . 5222))))

;; (require 'notify)

;; (defun notify-jabber-notify (from buf text proposed-alert)
;;   "(jabber.el hook) Notify of new Jabber chat messages via notify.el."
;;   (when (or jabber-message-alert-same-buffer
;;             (not (memq (selected-window) (get-buffer-window-list buf))))
;;     (if (jabber-muc-sender-p from)
;;         (notify (format "(PM) %s"
;;                         (jabber-jid-displayname (jabber-jid-user from)))
;;                 (format "%s: %s" (jabber-jid-resource from) text)))
;;     (notify (format "%s" (jabber-jid-displayname from))
;;             text)))

;; (require 'mu4e-alert)
;; (defun my/jabber-alert-set-window-urgency-maybe (from buf text proposed-alert)
;;   "Jabber alert - make the window blinking."
;;   (mu4e-alert-set-window-urgency-maybe))

;; (add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)
;; (add-hook 'jabber-alert-message-hooks 'my/jabber-alert-set-window-urgency-maybe)


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


(define-key global-map "\C-c\C-j" jabber-global-keymap)

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

(bind-key "C-c 2 g" 'my/projectile-switch-project-magit)
(bind-key "C-c 2 d" 'my/projectile-switch-project-dired)

(setq gc-cons-threshold 20000000)

(setq helm-M-x-fuzzy-match                  t
      helm-bookmark-show-location           t
      helm-buffers-fuzzy-matching           t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match           t
      helm-imenu-fuzzy-match                t
      helm-mode-fuzzy-match                 t
      helm-locate-fuzzy-match               t
      helm-quick-update                     t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t)

;; org-jira-mode
(setq jiralib-url "https://jira.tick42.com")

;; truncate-lines enabled by default
(set-default 'truncate-lines t)

(defun my/kmacro-end-and-call-macro (arg &optional no-repeat)
  "Call last keyboard macro, ending it first if currently being defined.
With numeric prefix ARG, repeat macro that many times.
Zero argument means repeat until there is an error.
To give a macro a permanent name, so you can call it
even after defining other macros, use \\[kmacro-name-last-macro]."
  (interactive "P")
  (if defining-kbd-macro
      (kmacro-end-macro nil))
  (progn
    (font-lock-mode -1)
    (kmacro-call-macro arg no-repeat)
    (font-lock-mode)))

(global-set-key [remap kbd-end-or-call-macro] 'my/kmacro-end-and-call-macro)

(fset 'my/send-to-jpm-mail [?\M-x ?c ?o ?p ?y ?- ?f ?i ?l ?e ?- ?n
                                  ?a ?m ?e ?- ?t ?o ?- ?c ?l ?i ?e backspace ?p return ?\M-x
                                  ?m ?u ?r ?3 backspace backspace ?4 ?3 backspace ?e ?- ?c ?o
                                  ?m ?p ?o ?s ?e return ?i ?v ?a ?n ?. ?y ?o ?n ?c ?h ?o ?v
                                  ?s ?k ?i ?@ ?j ?p tab ?\C-n ?\C-y ?\C-n ?\C-n ?\C-c ?\C-a
                                  ?\C-f backspace backspace ?\C-y return return return return
                                  ?\C-c ?\C-c])

(add-hook 'org-mode-hook #'org-bullets-mode)

(eval-after-load  "dired-x" '(defun dired-clean-up-after-deletion (fn)
                               "My. Clean up after a deleted file or directory FN.
Remove expanded subdir of deleted dir, if any."
                               (save-excursion (and (cdr dired-subdir-alist)
                                                    (dired-goto-subdir fn)
                                                    (dired-kill-subdir)))

                               ;; Offer to kill buffer of deleted file FN.
                               (if dired-clean-up-buffers-too
                                   (progn
                                     (let ((buf (get-file-buffer fn)))
                                       (and buf
                                            (save-excursion ; you never know where kill-buffer leaves you
                                              (kill-buffer buf))))
                                     (let ((buf-list (dired-buffers-for-dir (expand-file-name fn)))
                                           (buf nil))
                                       (and buf-list
                                            (while buf-list
                                              (save-excursion (kill-buffer (car buf-list)))
                                              (setq buf-list (cdr buf-list)))))))
                               ;; Anything else?
                               ))
;; ibuffer configuration
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("java" (mode . java-mode))
               ("w3m" (mode . w3m-mode))
               ("temporary" (name . "\*.*\*"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; python configuration
(require 'elpy)
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python")
(elpy-use-ipython "ipython")
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'pyvenv-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;;; packages.el ends here