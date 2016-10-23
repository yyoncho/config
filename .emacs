;;; .emacs -- generic command-dispatcher facility.  -*- lexical-binding: t -*-
;;; Commentary:
;;; my YY Emacs configuration
;;; Code:

(defconst emacs-start-time (current-time))

(package-initialize)

(require 'package)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Check if we're on Emacs 24.4 or newer, if so, use the pinned package feature
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((bm                 . "marmalade")
          (smex               . "melpa-stable")
          (zenburn-theme      . "melpa-stable")
          (anti-zenburn-theme . "melpa-stable")
          (zen-and-art-theme  . "marmalade")
          (cider              . "melpa-stable")
          (htmlize            . "marmalade")
          (rainbow-delimiters . "melpa-stable")
          (icicles            . "melpa"))))


(load "~/.emacs.d/init.el")

(toggle-truncate-lines t)
(prelude-require-package 'ido-vertical-mode)
(prelude-require-package 'ac-cider)
(prelude-require-package 'bind-key)
(prelude-require-package 'java-snippets)
(prelude-require-package 'elp)
(prelude-require-package 'clj-refactor)
(prelude-require-package 'helm-swoop)
(prelude-require-package 'highlight-symbol)
(prelude-require-package 'helm-descbinds)
(prelude-require-package 'flx-ido)
(prelude-require-package 'ido-ubiquitous)

(prelude-require-package 'midje-mode)
(prelude-require-package 'eclipse-theme)
(prelude-require-package 'flycheck-pos-tip)
(prelude-require-package 'flycheck-clojure)
(prelude-require-package 'auto-highlight-symbol)
(prelude-require-package 'aggressive-indent)
(prelude-require-package 'bm)
(prelude-require-package 'helm-projectile)
(prelude-require-package 'ujelly-theme)
(prelude-require-package 'golden-ratio)
(prelude-require-package 'back-button)
(prelude-require-package 'cider-eval-sexp-fu)
(prelude-require-package 'powerline)
(prelude-require-package 'switch-window)
(prelude-require-package 'recentf)
(prelude-require-package 'cider-eval-sexp-fu)
(prelude-require-package 'auto-complete-nxml)
(prelude-require-package 'sr-speedbar)
(prelude-require-package 'omnisharp)
(prelude-require-package 'dired+)


;; modes
(ido-mode t)
(ido-vertical-mode t)
(winner-mode 1)
(flx-ido-mode t)
(transient-mark-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(show-paren-mode 1)
(setq blink-matching-delay 0.1)
(global-subword-mode t)
(delete-selection-mode t)
(setq indent-tabs-mode nil)
(menu-bar-mode -1)
(undo-tree-mode t)
(global-whitespace-mode -1)
(global-hl-line-mode -1)
(global-auto-highlight-symbol-mode t)
(blink-cursor-mode t)
(recentf-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(helm-descbinds-mode 1)
(global-flycheck-mode t)
(flycheck-pos-tip-mode t)

(powerline-default-theme)

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; ido configuration

;; Do not list non user files
(defun ido-ignore-non-user-except-ielm (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except **.
NAME - the name of the buffer."
  (and (string-match "^\*" name)
       (not (and (string-match "repl" name)
                 (not (string-match "repl-messages" name))))
       (not (string-match "shell\\|ansi-term\\|magit\\|Magit\\|cider" name))))
(require 'ido)

(setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except-ielm))

(require 'ido-vertical-mode)

(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; magit configuration
(require 'magit)
(setq git-commit-summary-max-length 999)

(setq kill-do-not-save-duplicates t)

;; recent file configuration
(require 'recentf)
(setq recentf-max-menu-items 50)

(set-face-attribute 'default nil :height 110)

;; disable backup files
(setq make-backup-files nil)

(defun indent-buffer ()
  "Indent buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(custom-set-variables
 '(uniquify-buffer-name-style 'reverse)
 '(inhibit-startup-screen t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(send-mail-function (quote smtpmail-send-it))
 '(speedbar-show-unknown-files t)
 '(x-select-enable-clipboard t))

(require 'sgml-mode)
(setq sgml-basic-offset 4)

;;(setq c-basic-indent 2)
(setq tab-width 4)
(setq-default tab-width 4)

;; Descrnibe last command
(defun describe-last-function()
  (interactive)
  (describe-function last-command))

(set-face-attribute 'default nil :height 140)

;; dired files
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq delete-by-moving-to-trash t)
(setq dired-recursive-deletes 'always)
(setq dired-deletion-confirmer '(lambda (x) t))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let ((window (selected-window)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; eval and replace
(defun eval-and-replace ()
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
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; cider configuration
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'auto-complete-mode)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook #'aggressive-indent-mode)
(add-hook 'cider-mode-hook #'auto-highlight-symbol-mode)

(require 'cider)
(setq cider-test-show-report-on-success nil)
(setq cider-prompt-save-file-on-load 'always-save)
(setq cider-use-fringe-indicators t)

(require 'midje-mode)
(setq midje-comments ";;.;.")

(setq cider-auto-mode 't)
(eval-after-load 'cider-mode
  '(define-key cider-mode-map (kbd "C-x C-j") 'projectile-find-implementation-or-test-other-window))

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

(add-hook 'java-mode-hook #'paredit-mode)
(add-hook 'java-mode-hook #'java-conf)
(add-hook 'java-mode-hook #'meghanada-mode)
(add-hook 'java-mode-hook #'aggressive-indent-mode)
(add-hook 'java-mode-hook #'yas-minor-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(require 'cc-mode)

(eval-after-load 'java-mode
  '(define-key java-mode-map (kbd "C-x C-j") 'projectile-find-implementation-or-test-other-window))
(define-key java-mode-map (kbd "C-<f11>") 'meghanada-run-junit-recent)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun go-to-terminal-window ()
  "Go to terminal window."
  (interactive)
  (switch-to-buffer "*ansi-term*"))

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
(require 'clj-refactor)

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

(bind-key "C-x 2" 'my/vsplit-last-buffer)
(bind-key "C-x 3" 'my/hsplit-last-buffer)

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

;; god more configuration
(require 'god-mode)
(defun my-update-cursor ()
  "Update my cursor."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar))
  (set-cursor-color (if (or god-local-mode buffer-read-only)
                        "yellow"
                      "white")))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

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

(add-hook 'xml-mode-hook
          (lambda()
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

(defun my/mvn-dependency-version-to-properties ()
  "Move dependency version to properties section."
  (interactive "p")
  (save-excursion
    (search-forward "<version>")
    (kill-region (point) (progn
                           (search-forward "</version>"
                                           nil nil nil)
                           (backward-char 10)
                           (point)))
    (let ((version (car kill-ring-yank-pointer)))
      (search-backward "<dependency>")
      (search-forward "<artifactId>")
      (kill-ring-save (point) (progn
                                (search-forward "</artifactId>"
                                                nil nil nil)
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

(defun my/projectile-open-pom ()
  "Open's pom file from the project."
  (interactive)
  (find-file (concat (projectile-project-root) "pom.xml")))

(defun my/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(require 'vc-dispatcher)
(setq vc-suppress-confirm nil)

;; helm configuration
(require 'helm)
(require 'projectile)
(require 'cider-eval-sexp-fu)

(defun my/other-window ()
  "Select other window or switch buffer if there is only one window."
  (interactive)
  (let ((old-window  (selected-window)))
    (other-window 1)
    (when (equal old-window (selected-window))
      (switch-to-buffer (next-buffer)))))

(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

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

(defun my/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

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
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun my/magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)


(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; clipboard

;; unset the suspend frame command
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-c I"))

;; global key configuration
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "<C-f8>") 'bm-toggle)
(global-set-key (kbd "<f8>")   'bm-next)
(global-set-key (kbd "<M-f8>") 'bm-previous)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "<f7>") 'go-to-terminal-window)
(global-set-key (kbd "<f11>") 'fullscreen)
(global-set-key (kbd "M-j") 'join-next-line)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-l") 'other-window)
(global-set-key (kbd "C-M-u") 'er/expand-region)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-s") 'helm-swoop)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-S-l") 'helm-projectile-ack)
(global-set-key (kbd "C-S-c") 'comment-region)
(global-set-key (kbd "C-v") 'ace-window)
(global-set-key (kbd "C-c 9") 'buffer-menu)
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)
(global-set-key (kbd "C-M-u") 'er/expand-region)
(global-set-key (kbd "C-x 9") 'helm-locate)
(global-set-key (kbd "C-<backspace>") 'subword-backward-kill)
(global-set-key (kbd "C-x v") 'eval-buffer)
(global-set-key (kbd "C-c C-c") 'eval-defun)
(global-set-key (kbd "C-c h") 'helm-google-suggest)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-c p x p") 'my/projectile-open-pom)
(global-set-key (kbd "C-x C-j") 'projectile-find-implementation-or-test-other-window)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "M-<left>") 'back-button-global-backward)
(global-set-key (kbd "M-<right>") 'back-button-global-forward)
(global-set-key (kbd "C-c 1") 'switch-window)
(global-set-key (kbd "<f6>") 'god-mode)
(global-set-key (kbd "<f7>") 'sr-speedbar-toggle)
(global-set-key (kbd "C-x o") 'my/other-window)
(global-set-key (kbd "C-x d") 'dire)
(global-set-key (kbd "C-v") 'change-inner)
(global-set-key (kbd "M-v") 'copy-inner)
(global-set-key (kbd "C-c I") 'my/find-user-init-file)

(require 'auto-complete-nxml)
;; Keystroke to popup help about something at point.
(setq auto-complete-nxml-popup-help-key "C-:")
;; Keystroke to toggle on/off automatic completion.
(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)

;;; .emacs ends here
