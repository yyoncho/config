;;; my-emacs-lisp.el --- Emacs Lisp configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan

;; Author: Ivan <kyoncho@myoncho>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Emacs lisp
(require 'company)
(require 'core-keybindings)
(require 'rainbow-delimiters)
(require 'evil-smartparens)
(require 'evil-cleverparens)
(require 'flycheck)

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "," 'eval-defun)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "riv" 'emr-el-inline-variable)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "rlv" 'emr-el-extract-variable)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "ril" 'emr-el-extract-to-let)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "ref" 'emr-el-extract-function)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "rfe" 'emr-el-implement-function)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "ris" 'emr-el-inline-let-variable)

(let ((ssdf (let () "sd")))
  ssdf)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (delete 'company-capf company-backends)))

(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'emr-initialize)


(provide 'my-emacs-lisp)
;;; my-emacs-lisp.el ends here
