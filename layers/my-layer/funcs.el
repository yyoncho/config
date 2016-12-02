;(require 'sx)

(defun my/init ()
  (interactive)
  (require 'magit)
  (setq git-commit-summary-max-length 999)
  (setq kill-do-not-save-duplicates t)

  ;; disable backup files
  (setq make-backup-files nil)

  (global-diff-hl-mode t)

  (custom-set-variables
   '(ediff-split-window-function (quote split-window-horizontally))
   '(send-mail-function (quote smtpmail-send-it)))

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

  (add-hook 'cider-mode-hook #'auto-highlight-symbol-mode)

  (setq cider-test-show-report-on-success nil)
  (setq cider-prompt-save-file-on-load 'always-save)
  (setq cider-use-fringe-indicators t)

  (require 'midje-mode)
  (setq midje-comments ";;.;.")
  (setq cider-auto-mode 't)

  (eval-after-load 'cider-mode
    '(define-key cider-mode-map (kbd "C-c M-r") 'cider-restart))

  (require 'meghanada)
  (add-hook 'java-mode-hook #'java-conf)
  (add-hook 'java-mode-hook #'aggressive-indent-mode)
  (add-hook 'java-mode-hook #'yas-minor-mode)

  (define-key java-mode-map (kbd "C-x C-j")
    'projectile-find-implementation-or-test-other-window)

  (define-key clojure-mode-map (kbd "C-x C-j")
    'projectile-find-implementation-or-test-other-window)

  (define-key java-mode-map (kbd "C-<f11>") 'meghanada-run-junit-recent)

  (defun kill-current-buffer ()
    "Kill current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

  (setq make-backup-files nil)

  (defun join-next-line ()
    "Joins the next line into the current one."
    (interactive)
    (save-excursion
      (move-end-of-line nil)
      (delete-char 1)
      (delete-horizontal-space)))

  (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

  (setq cider-lein-command "~/.bin/lein")
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)

  (setenv "PATH" (concat (getenv "PATH") ":~/.bin"))
  (put 'set-goal-column 'disabled nil)

  (require 'ediff-diff)

  (setq ediff-diff-options "-w")

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

  ;; prevent prompt for killing emcacsclient buffer
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
  (setq inhibit-splash-screen t)

  (require 'term)
  (define-key term-raw-map  (kbd "C-'") 'term-line-mode)
  (define-key term-mode-map (kbd "C-'") 'term-char-mode)
  (define-key term-raw-map  (kbd "C-y") 'term-paste)

  (require 'god-mode)
  (define-key god-local-mode-map (kbd ".") 'repeat)
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

  (setq shift-selection-mode t)

  (defun my/remove-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

  ;; auto complete settings
  (require 'auto-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  (setq sgml-basic-offset 4)

  (add-hook 'nxml-mode-hook
            (lambda()
              (web-mode t)
              (local-unset-key (kbd "C-M-u"))))

  (require 'browse-url)
  (setq browse-url-browser-function 'browse-url
        browse-url-generic-program "eww")

  (set-face-attribute 'default nil :height 130)

  (require 'flycheck)
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  (add-hook 'emacs-lisp-mode-hook       #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook             #'aggressive-indent-mode)

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

  (require 'company)
  (define-key company-active-map "\C-p" 'company-select-previous)
  (define-key company-active-map "\C-n" 'company-select-next)
  (define-key company-active-map "\C-j" 'company-complete-selection)

  (require 'cc-mode)
  (define-key java-mode-map "\M-j" 'join-next-line)

  (add-hook 'midje-mode-hook
            (lambda ()
              (define-key midje-mode-map (kbd "C-c b") nil)))

  ;; always follow symlinks
  (setq vc-follow-symlinks t)

  ;; use windows-1251
  (modify-coding-system-alist 'file "\\.txt\\'" 'windows-1251)

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

  (defun my/copy-file-name-to-clipboard ()
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



  (require 'vc-dispatcher)
  (setq vc-suppress-confirm nil)

  (defun my/other-window (arg)
    "Select ARGth window or switch buffer if there is only one window."
    (interactive "p")
    (let ((old-window  (selected-window)))
      (other-window arg)
      (when (equal old-window (selected-window))
        (other-frame arg))))

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
  (bind-key "C-x k" 'kill-current-buffer)
  (bind-key "M-j" 'join-next-line)
  (bind-key "C-x C-r" 'helm-recentf)
  (bind-key "M-/" 'hippie-expand)
  (bind-key "C-M-u" 'er/expand-region)
  (bind-key "C->" 'mc/mark-next-like-this)
  (bind-key "C-<" 'mc/mark-previous-like-this)
  (bind-key "C-c C-<" 'mc/mark-all-like-this)
  (bind-key "C-h" 'backward-delete-char)
  (bind-key "C-M-h" 'backward-kill-word)
  (bind-key "C-M-y" 'helm-show-kill-ring)
  (bind-key "C-S-l" 'helm-projectile-ack)
  (bind-key "C-S-c" 'comment-region)
  (bind-key "C-M-u" 'er/expand-region)
  (bind-key "C-<backspace>" 'subword-backward-kill)
  (bind-key "C-c C-c" 'eval-defun)
  (bind-key "C-c h" 'helm-google-suggest)
  (bind-key "C-x C-j" 'my/projectile-find-implementation)
  (bind-key "C->" 'mc/mark-next-like-this)
  (bind-key "C-<" 'mc/mark-previous-like-this)
  (bind-key "<f6>" 'god-mode)
  (bind-key "<f7>" 'sr-speedbar-toggle)
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
  (bind-key "C-c M-p" 'my/projectile-open-pom)

  (require 'nxml-mode)
  (bind-key "C-c M-e" 'my/mvn-dependency-version-to-properties nxml-mode-map)

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

  (setq magit-save-repository-buffers 'dontask)

  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  (require 'mu4e-speedbar)

  (defun my/mu4e-go-to-inbox ()
    "Go to inbox."
    (interactive)
    (mu4e-headers-search
     (format "maildir:\"%s\"" "/INBOX")))

  (setq mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent Items"
        mu4e-trash-folder  "/Trash"
        mu4e-msg2pdf "/usr/bin/msg2pdf"
        mu4e-update-interval 200)

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'sent)

  (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?j)
          ("/Drafts" . ?d)
          ("/Trash" . ?t)
          ("/Sent Items" . ?s)
          ("/bamboo" . ?b)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; something about ourselves
  (setq
   user-mail-address "ivan.yonchovski@tick42.com"
   user-full-name  "Ivan Yonchovski"
   mu4e-compose-signature nil)

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.office365.com" 587 "ivan.yonchovski@tick42.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 587)

  (setq message-kill-buffer-on-exit t)

  (use-package mu4e-alert
    :ensure t
    :config
    (mu4e-alert-enable-notifications)
    (mu4e-alert-set-default-style 'libnotify)
    (setq mu4e-alert-interesting-mail-query
          (concat "maildir:/INBOX and flag:unread"))

    (alert-add-rule
     :category "mu4e-alert"
     :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
     :continue )

    ;; display stuff on modeline as well as notify
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

  ;; elfeed configuration
  (require 'elfeed)
  (setq elfeed-feeds
        '("http://sachachua.com/blog/feed/"
          "http://feeds.feedburner.com/cyclingnews/news?format=xml"
          "http://endlessparentheses.com/atom.xml"))

  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (emms-mode-line -1)

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

  (defun notify-jabber-notify (from buf text proposed-alert)
    "(jabber.el hook) Notify of new Jabber chat messages via notify.el."
    (when (or jabber-message-alert-same-buffer
              (not (memq (selected-window) (get-buffer-window-list buf))))
      (if (jabber-muc-sender-p from)
          (inotify (format "(PM) %s"
                           (jabber-jid-displayname (jabber-jid-user from)))
                   (format "%s: %s" (jabber-jid-resource from) text)))
      (notify (format "%s" (jabber-jid-displayname from))
              text)))

  (require 'mu4e-alert)
  (defun my/jabber-alert-set-window-urgency-maybe (from buf text proposed-alert)
    "Jabber alert - make the window blinking."
    (mu4e-alert-set-window-urgency-maybe))

  (add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)
  (add-hook 'jabber-alert-message-hooks 'my/jabber-alert-set-window-urgency-maybe)

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
  (bind-key "C-=" 'er/expand-region)
  (bind-key "C-c 2 d" 'my/projectile-switch-project-dired)

  (setq gc-cons-threshold 20000000)

  (setq helm-M-x-fuzzy-match                  t
        helm-bookmark-show-location           t
        helm-buffers-fuzzy-matching           t
        helm-completion-in-region-fuzzy-match t
        helm-file-cache-fuzzy-match           t
        helm-imenu-fuzzy-match                t
        helm-mode-fuzzy-match                 t
        helm-locate-fuzzy-match               nil
        helm-quick-update                     t
        helm-recentf-fuzzy-match              t
        helm-semantic-fuzzy-match             t)

  ;; org-jira-mode
  (setq jiralib-url "https://jira.tick42.com")

  ;; truncate-lines enabled by default
  (set-default 'truncate-lines t)



  (global-set-key [remap kbd-end-or-call-macro] 'my/kmacro-end-and-call-macro)

  (fset 'my/send-to-jpm-mail [?\M-x ?c ?o ?p ?y ?- ?f ?i ?l ?e ?- ?n
                                    ?a ?m ?e ?- ?t ?o ?- ?c ?l ?i ?e backspace ?p return ?\M-x
                                    ?m ?u ?r ?3 backspace backspace ?4 ?3 backspace ?e ?- ?c ?o
                                    ?m ?p ?o ?s ?e return ?i ?v ?a ?n ?. ?y ?o ?n ?c ?h ?o ?v
                                    ?s ?k ?i ?@ ?j ?p tab ?\C-n ?\C-y ?\C-n ?\C-n ?\C-c ?\C-a
                                    ?\C-f backspace backspace ?\C-y return return return return
                                    ?\C-c ?\C-c])

  (add-hook 'org-mode-hook #'org-bullets-mode)

  (eval-after-load  "dired-x"
    '(defun dired-clean-up-after-deletion (fn)
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
  (global-set-key [remap eww-follow-link] 'my/eww-follow-link)

  (defun my/browse-url (url new-window)
    "Browse url in the associated app.
URL - the url to browse.
new-window - whether to open in new window."
    (let ((host (elt (url-generic-parse-url url) 4)))
      (if (or (string-equal "stackoverflow.com" host)
              (s-index-of "stackexchange.com" host))
          (sx-open-link url)
        (eww-follow-link))))

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
  (setq sx-question-mode-display-buffer-function #'pop-to-buffer-same-window)

  ;; use mobile interface
  (setq url-user-agent (concat
                        "User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) "
                        "AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7\n"))

  ;; persistent-scratch

  (persistent-scratch-setup-default)
  (require 'cc-mode)
  (add-hook 'java-mode-hook #'meghanada-mode)
  (fset 'my/copy-worklog
        [?\C-c ?\C-x ?\C-w ?\C-y ?\C-y ?\C-p tab ?\C-n tab ?\C-n ?\C-k ?\C-k ?\C-n ?\M-f ?\M-f ?\M-f ?\M-f ?\C-f ?\M-x ?m ?y ?- backspace ?/ ?i ?n tab return])

  (custom-set-variables
   '(jabber-alert-muc-hooks nil)
   '(jabber-alert-presence-hooks nil)
   '(jabber-mode-line-compact t)
   '(jabber-mode-line-mode nil)
   '(mu4e-hide-index-messages t)
   '(bmkp-last-as-first-bookmark-file "~/.emacs.d/savefile/bookmarks")
   '(excorporate-configuration
     (quote
      ("ivan.yonchovski@tick42.com" . "https://pod51036.outlook.com/ews/Exchange.asmx")))
   '(global-auto-highlight-symbol-mode t)
   '(global-command-log-mode t)
   '(projectile-globally-ignored-files
     (quote ("TAGS" ".lein-repl-history")))
   '(projectile-globally-ignored-directories
     (quote (".idea" ".ensime_cache" ".eunit" "target" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "target" ))))

  (define-key calendar-mode-map (kbd  "<f2>") #'exco-calendar-show-day)


  (bind-key "C-x C-j" 'my/projectile-find-implementation)
  (jabber-mode-line-mode t)
  (helm-flx-mode t)
  (setq evil-move-beyond-eol t)

  (defun my/projectile-find-implementation ()
    "Open matching implementation or test file in other window."
    (interactive)
    (find-file (projectile-find-implementation-or-test (buffer-file-name))))

  (load "soap-client.el")

  (require 'sx-interaction)

  (defun my/emms-start ()
    "Start emms."
    (interactive)
    (emms-default-players)
    (emms-add-directory-tree "~/Music")
    (emms-random))

  (defun my/avy-goto-char-3 (char1 char2 char3 &optional arg beg end)
    "Jump to the currently visible CHAR1 followed by CHAR2 and char3.
The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive (list (read-char "char 1: " t)
                       (read-char "char 2: " t)
                       (read-char "char 3: " t)
                       current-prefix-arg
                       nil nil))
    (when (eq char1 ?)
      (setq char1 ?\n))
    (when (eq char2 ?)
      (setq char2 ?\n))
    (when (eq char1 ?)
      (setq char1 ?\n))
    (avy-with avy-goto-char-2
      (avy--generic-jump
       (regexp-quote (string char1 char2 char3))
       arg
       avy-style
       beg end))))
