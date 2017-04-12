(defconst my-layer-packages
  '(ac-cider
    ace-link
    auto-complete-nxml
    flycheck-clojure
    flycheck-pos-tip
    java-snippets
    persistent-scratch
    evil-smartparens))

(defun my-layer/init-sx ()
  (use-package sx
    :defer nil
    :init
    (progn
      (require 'sx-interaction))
    :config
    (progn
      )))
;;; packages.el ends here
