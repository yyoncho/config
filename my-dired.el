(require 'dired)
(require 'dired+)
(require 'dired-x)
(require 'dired-subtree)

(define-key dired-mode-map (kbd "TAB") (lambda ()
                                         (interactive)
                                         (dired-subtree-toggle)
                                         (revert-buffer)))

(define-key dired-mode-map (kbd "E") 'dired-view-file)
;; Buffer-local variable


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
                    (setq buf-list (cdr buf-list)))))))))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
      delete-by-moving-to-trash t
      dired-recursive-deletes 'always
      dired-deletion-confirmer '(lambda (x) t))

(diredp-toggle-find-file-reuse-dir 1)

(setq-default dired-listing-switches "-aBhl  --group-directories-first"
              dired-omit-files-p t)
