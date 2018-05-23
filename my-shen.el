;;; my-shen.el --- Shen configuration                -*- lexical-binding: t; -*-

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

;; shen configuration

;;; Code:

(add-to-load-path "/home/kyoncho/Sources/shen/shen-mode/")

(require 'shen-mode)
(require 'inf-shen)

(spacemacs/set-leader-keys-for-major-mode 'shen-mode
  "," 'shen-eval-defun
  "ee" 'shen-eval-last-sexp
  "ss" 'switch-to-shen)
(add-hook 'shen-mode-hook 'evil-smartparens-mode)
(add-hook 'shen-mode-hook 'evil-cleverparens-mode)

(provide 'my-shen)
;;; my-shen.el ends here
