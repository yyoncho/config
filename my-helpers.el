;;; my-helpers.el --- Helper functions               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>
;; Keywords: abbrev, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mu4e)

(defun my/send-to-jpm ()
  "Sends current file to JPM"
  (interactive)
  (let ((file-name (buffer-file-name))
        (short-name (buffer-name)))
    (when (equal (file-name-extension file-name) "jar")
      (setq file-name (concat file-name ".zip"))
      (copy-file (buffer-file-name) file-name t))
    (save-excursion
      (mu4e-compose-new)
      (erase-buffer)
      (insert-file "~/.send-template")
      (replace-string "{file-name}" file-name)
      (beginning-of-buffer)
      (replace-string "{short-name}" short-name)
      (message-send-and-exit))))

(provide 'my-helpers)
;;; my-helpers.el ends here
