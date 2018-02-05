
(setq meghanada-javac-xlint "-Xlint:-processing")
(spacemacs|define-jump-handlers java-mode (meghanada-jump-declaration :async t))

(require 'flycheck-meghanada)
(meghanada-flycheck-enable)

(defun my/meghanada-local-variable ()
  (interactive)
  (save-buffer)
  (meghanada-local-variable))

(defun my/run-test ()
  (interactive)
  (save-buffer)
  (meghanada-compile-project)
  (meghanada-run-junit-test-case))

(defun my/run-test-class ()
  (interactive)
  (save-buffer)
  (meghanada-compile-file)
  (meghanada-run-junit-class))

(defun my/run-test-recent ()
  (interactive)
  (save-buffer)
  (meghanada-compile-file)
  (meghanada-run-junit-recent))

(spacemacs/set-leader-keys-for-major-mode 'java-mode
  "tt" 'meghanada-run-junit-test-case
  "ed" 'meghanada-debug-junit-class
  "," 'meghanada-run-junit-test-case
  "rtf" 'jr-convert-to-field
  "xm" 'meghanada-exec-main
  "raf" 'jr-add-field
  "rcl" 'my/cycle-log-level
  "rC" 'jr-create-constructor-from-fields
                                        ;"rai" 'meghanada-import-all
  "rfu" 'meghanada-reference
  "rlv" 'my/meghanada-local-variable
  "rat" 'my/add-throws
  "rrm" 'my/rename-current-method
                                        ; "rl" 'meghanada-local-variable
  "tg" 'meghanada-run-junit-recent
                                        ; "rcf" 'my/create-constructor-from-fields
  "ea" (lambda ()
         (interactive)
         (projectile-save-project-buffers)
         (meghanada-compile-project))
  "eb" 'meghanada-compile-file
  "ab" 'java-ide-add-breakpoint
  "dc" 'java-ide-debug-class
  "dt" 'java-ide-debug-test
  "fp" 'my/find-pom-file
  "qr" 'meghanada-restart
  "tn" 'meghanada-run-junit-class)

(defun my/add--field-to-current-class (var-data)
  (semantic-go-to-tag (semantic-current-tag-of-class 'type))
  (re-search-forward "{")
  (newline-and-indent)
  (insert-string "private " (plist-get (third var-data) :type ) " " (first var-data) ";"))


(defun my/meghanada-restart ()
  (with-current-buffer
      (first (-filter (lambda (b) (s-ends-with? "java" (buffer-name b)))
                      (buffer-list )))
    (meghanada-restart)))


;; (setq my/compilation-index 0)

;; (defun my/java-save-buffer ()
;;   (when (s-equals? (file-name-extension (buffer-file-name)) "java")
;;     (let ((default-directory (locate-dominating-file (buffer-file-name) "pom.xml")))
;;       (message "Start building ...")
;;       (compile "mvn install -o -DskipTests")
;;       (with-current-buffer "*compilation*"
;;         (rename-buffer (s-concat "compilation" (number-to-string (setq my/compilation-index (1+ my/compilation-index)))))))))

;; (add-hook 'file-save-hook 'my/java-save-buffer)
(require 'flycheck-meghanada)

;; (remove-hook 'java-mode-hook #'aggressive-indent-mode)
(add-hook 'java-mode-hook #'yas-minor-mode)
(add-hook 'java-mode-hook (lambda () (auto-complete-mode -1)))
(add-hook 'java-mode-hook #'evil-cleverparens-mode)
(add-hook 'java-mode-hook #'my/configure-java)
;; (add-hook 'java-mode-hook #'meghanada-mode)
(add-hook 'java-mode-hook #'meghanada-flycheck-enable)
(add-hook 'java-mode-hook #'flycheck-mode)

(require 'cc-mode)
(define-key java-mode-map (kbd "C-M-i") 'company-manual-begin)
