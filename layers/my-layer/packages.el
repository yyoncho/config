(defconst my-layer-packages
  '(auto-complete-nxml
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
