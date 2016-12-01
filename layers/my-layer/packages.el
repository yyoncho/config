(defconst my-layer-packages
  '(ac-cider
    magit
    ace-link
    auto-complete-nxml
    crux
    dired+
    dired-efap
    dired-explorer
    dired-subtree
    elpy
    emms
    flycheck-clojure
    flycheck-pos-tip
    god-mode
    helm-dash
    java-snippets
    meghanada
    midje-mode
    persistent-scratch
    sr-speedbar
    sx
    zenburn-theme))

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
