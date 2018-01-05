(defun my/goto-imports-start ()
  (goto-char (point-min))
  (let ((package-point (re-search-forward "package .*;" nil t))
        (import-point (re-search-forward "import .*;" nil t)))
    (cond (import-point (goto-char import-point)
                        (beginning-of-line))
          (package-point (goto-char package-point)
                         (forward-line)
                         (open-line 2)
                         (forward-line))
          (t (goto-char (point-min))
             (open-line 1)))))

(defun my/import-name (imp)
  (let ((case-fold-search nil))
    (let ((imp (when (string-match "\\([a-z0-9_]+\\.\\)+[A-Za-z0-9_]+" imp)
                 (match-string 0 imp))))
      (prog1
          imp))))

(defun my/import-exists-p (imp)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^import\\s-+" imp "\\s-*;") nil t)))

(defun my/add-import (import-name)
  (save-mark-and-excursion
   (unless (or (string-prefix-p "java.lang." import-name)
               (meghanada--import-exists-p import-name))
     (let ((start t))
       (my/goto-imports-start)
       (while (and start (re-search-forward "^import .+;" nil t))
         (forward-line)
         (setq start (/= (point-at-bol) (point-at-eol))))
       (insert (format "import %s;\n" import-name))))))

(defun my/get-class-fields ()
  (-map
   (lambda (item) (list (plist-get (third item) :type) (first item) ))
   (-filter
    (lambda (item) (eq (second item) 'variable))
    (plist-get (third sem-scope) :members))))

(defun my/create-constructor-from-fields ()
  "Create constructor from fields"
  (interactive)
  (semantic-force-refresh)
  (let* ((sem-scope (semantic-current-tag))
         (p (point))
         (fields (my/get-class-fields)))
    (newline-and-indent)
    (insert-string
     "public "
     (first sem-scope)
     "("
     (s-join ", "
             (-map (lambda (i) (s-concat (first i) " " (second i)))
                   fields))
     ")")
    (newline-and-indent)
    (insert-string "{")
    (-each fields
      (lambda (f)
        (newline-and-indent)
        (insert-string "this." (second f) " = " (second f) ";")))
    (newline-and-indent)
    (insert-string "}")
    (indent-region p (point)))
  )

(setq meghanada-javac-xlint "-Xlint:-processing")
(spacemacs|define-jump-handlers java-mode (meghanada-jump-declaration :async t))


(defun my/meghanada-local-variable ()
  (interactive)
  (save-buffer)
  (meghanada-local-variable))

(defun my/run-test ()
  (interactive)
  (save-buffer)
  (meghanada-compile-file)
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
  "fp" 'my/find-pom-file
  "qr" 'meghanada-restart
  "tn" 'meghanada-run-junit-class)

(defun my/add--field-to-current-class (var-data)
  (semantic-go-to-tag (semantic-current-tag-of-class 'type))
  (re-search-forward "{")
  (newline-and-indent)
  (insert-string "private " (plist-get (third var-data) :type ) " " (first var-data) ";"))

(defun my/convert-to-field ()
  (interactive)

  (save-mark-and-excursion
   (semantic-force-refresh)
   (if-let ((variable (thing-at-point 'symbol t))
            (var-data (-first
                       (lambda (it)
                         (s-equals? (first it) variable))
                       (semantic-get-all-local-variables))))
       (semantic-go-to-tag var-data)
     (let ((start (point)))
       (re-search-forward (plist-get (third var-data) :type))
       (delete-region start (point))
       (indent-according-to-mode))
     (my/add--field-to-current-class var-data))))


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

(remove-hook 'java-mode-hook #'aggressive-indent-mode)
(add-hook 'java-mode-hook #'yas-minor-mode)
(add-hook 'java-mode-hook (lambda () (auto-complete-mode -1)))
(add-hook 'java-mode-hook #'spacemacs/toggle-evil-cleverparens-on)
(add-hook 'java-mode-hook #'my/configure-java)
(add-hook 'java-mode-hook #'meghanada-mode)
(add-hook 'java-mode-hook #'flycheck-mode)
