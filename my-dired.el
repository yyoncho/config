(require 'dired)
(require 'dired+)
(require 'dired-x)
(require 'dired-subtree)
(require 'dired-sidebar)

(define-key dired-mode-map (kbd "TAB") (lambda ()
                                         (interactive)
                                         (dired-subtree-toggle)
                                         (dired-revert)))

(define-key dired-mode-map (kbd "E") 'dired-view-file)
(define-key dired-mode-map (kbd "<f5>") (lambda ()
                                          (interactive)
                                          (dired-revert)))

(eval-after-load "dired-x"
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
                    (setq buf-list (cdr buf-list)))))))))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
      delete-by-moving-to-trash t
      dired-recursive-deletes 'always
      dired-deletion-confirmer '(lambda (x) t)
      dired-sidebar-stale-buffer-time-idle-delay 2
      dired-sidebar-follow-file-idle-delay 0.5
      dired-sidebar-should-follow-file t)

(diredp-toggle-find-file-reuse-dir 1)

(setq-default dired-listing-switches "-aBhl  --group-directories-first"
              dired-omit-files-p t)
(remove-hook 'dired-subtree-after-insert-hook (lambda () (dired-revert)))


(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|GPATH\\|GTAGS\\|GSYMS\\|GRTAGS")

;; (require 'savehist)
;; (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
;; (savehist-mode 1)

;; (with-eval-after-load 'dired
;;   (require 'helm-dired-history)
;;   ;; if you are using ido,you'd better disable ido for dired
;;   (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
;;   (require 'savehist)
;;   (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
;;   (savehist-mode 1)

;;   (with-eval-after-load 'dired
;;     (require 'helm-dired-history)
;;     ;; if you are using ido,you'd better disable ido for dired
;;     ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
;;     (define-key dired-mode-map (kbd "<f6>") 'dired)))
