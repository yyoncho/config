(defconst emacs-start-time (current-time))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

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
          (clojure-mode       . "melpa-stable")
          (htmlize            . "marmalade")
          (rainbow-delimiters . "melpa-stable")
          (icicles            . "melpa"))))


(load "~/.emacs.d/init.el")

(toggle-truncate-lines t)
(prelude-require-package 'ido-vertical-mode)
(prelude-require-package 'ac-cider)
(prelude-require-package 'bind-key)
(prelude-require-package 'java-snippets)
(prelude-require-package 'tabbar)
(prelude-require-package 'elpy)
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
(prelude-require-package 'recentf)
(prelude-require-package 'cider-eval-sexp-fu)

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
(golden-ratio-mode t)
(global-auto-highlight-symbol-mode t)
(blink-cursor-mode t)
(recentf-mode 1)
(tabbar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(helm-descbinds-mode 1)
(global-flycheck-mode t)
(flycheck-pos-tip-mode t)


(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; ido configuration
;; Do not list non user files
(defun ido-ignore-non-user-except-ielm (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except **."
  (and (string-match "^\*" name)
       (not (and (string-match "repl" name)
                 (not (string-match "repl-messages" name))))
       (not (string-match "shell\\|ansi-term\\|magit\\|Magit\\|cider" name))))

(setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except-ielm))
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; magit configuration
(setq magit-stage-all-confirm nil)
(setq git-commit-summary-max-length 999)

(setq kill-do-not-save-duplicates t)

;; recent file configuration
(setq recentf-max-menu-items 50)

(set-face-attribute 'default nil :height 110)

;; disable backup files
(setq make-backup-files nil)

;;; SLIME configuration
(setq inferior-lisp-program "/usr/bin/sbcl")

(defun indent-buffer ()
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

(setq sgml-basic-offset 4)

(setq c-basic-indent 2)
(setq tab-width 4)
(setq-default tab-width 4)

;; Descrnibe last command
(defun describe-last-function()
  (interactive)
  (describe-function last-command))

(set-face-attribute 'default nil :height 140)

;; skip .dot files
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; dired configuration
(setq delete-by-moving-to-trash t)
(setq dired-recursive-deletes 'always)

;; dired - no confirmation when deleting
(setq dired-deletion-confirmer '(lambda (x) t))

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

(setq cider-test-show-report-on-success nil)
(setq cider-prompt-save-file-on-load 'always-save)
(setq cider-use-fringe-indicators t)
(setq midje-comments ";;.;.")

(setq cider-auto-mode 't)
(eval-after-load 'cider-mode
  '(define-key cider-mode-map (kbd "C-x C-j") 'projectile-find-implementation-or-test-other-window))

(eval-after-load 'cider-mode
  '(define-key cider-mode-map (kbd "C-c M-r") 'cider-restart))

;; java configuration
(defun java-conf () 
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

(eval-after-load 'java-mode
  '(define-key java-mode-map (kbd "C-x C-j") 'projectile-find-implementation-or-test-other-window))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))


(defun kill-current-buffer ()
  "Kills current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun go-to-terminal-window ()
  "Goes to terminal window"
  (interactive)
  (switch-to-buffer "*ansi-term*"))

;; prevent creating backup files
(setq make-backup-files nil)

(defun join-next-line ()
  "Joins the next line into the current one."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (delete-forward-char 1)
    (delete-horizontal-space)))


(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(defun un-camelcase-word-at-point ()
  "un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.
The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore.
"
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
(setq ediff-diff-options "-w")

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; better window splitting
(defun my/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

(defun my/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
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
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; god more configuration
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar))
  (set-cursor-color (if (or god-local-mode buffer-read-only)
                        "yellow"
                      "white")))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

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

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")


(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(set-face-attribute 'default nil :height 130)

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

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


;; unset the suspend frame command
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


;; configuration for multiple cursors

(define-key global-map (kbd "C-c SPC") 'avy-goto-word-1)


(setq exec-path (append exec-path '("/usr/bin")))

(define-key company-active-map "\C-p" 'company-select-previous)
(define-key company-active-map "\C-n" 'company-select-next)
(define-key company-active-map "\C-j" 'company-complete-selection)

(define-key java-mode-map "\M-j" 'join-line)

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

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
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

(defun mvn-dependency-version-to-properties (&optional arg)
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

(defun mvn-inline-property (&optional arg)
  (interactive "p")
  (save-excursion
    (search-forward "<")
    (kill-region (point) (progn
                           (search-forward ">"
                                           nil nil arg)
                           (backward-char 1)
                           (point)))
    (let ((property-name (car kill-ring-yank-pointer)))
      (forward-char 1)
      (kill-region (point) (progn
                             (search-forward "<"
                                             nil nil arg)
                             (backward-char 1)
                             (point)))

      (let ((property-value (car kill-ring-yank-pointer)))
        (beginning-of-line)
        (kill-line)
        (kill-line)
        (replace-string (concat "${" property-name "}") property-value)))))

(defun mvn-sort-properties (&optional arg)
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

(setq vc-suppress-confirm nil)
;; helm configuration
(setq helm-grep-ignored-files (add-to-list 'helm-grep-ignored-files "*.war"))
(setq helm-grep-ignored-files (add-to-list 'helm-grep-ignored-files "*.class"))
(setq helm-grep-ignored-files (add-to-list 'helm-grep-ignored-files "*.zip"))
(setq helm-grep-ignored-files (add-to-list 'helm-grep-ignored-files ".classpath"))
(setq helm-grep-ignored-files (add-to-list 'helm-grep-ignored-files "*.jar"))
(setq grep-find-ignored-files helm-grep-ignored-files)
(setq grep-find-ignored-directories (add-to-list 'grep-find-ignored-directories ".meghanada"))


(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; global key configuration
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
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
(global-set-key (kbd "C-x C-j") 'projectile-find-implementation-or-test-other-window)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "M-<left>") 'back-button-global-backward)
(global-set-key (kbd "M-<right>") 'back-button-global-forward)
(global-set-key (kbd "<f6>") 'god-mode)


