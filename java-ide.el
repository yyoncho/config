;;; java-ide.el --- JAVA IDE                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:
;; (defun read-lines (filePath)
;;   "Return a list of lines of a file at filePath."
;;   (with-temp-buffer
;;     (insert-file-contents filePath)
;;     (split-string (buffer-string) "\n" t)))
(require 'realgud)

(defun java-ide--add-line (string file)
  "Add STRING as a line to FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (unless (re-search-forward (concat "^" (regexp-quote string) "$") nil t)
      (goto-char (point-max))
      (insert (concat "\n" string))
      (write-region (point-min) (point-max) file))))

(defun java-ide-add-breakpoint ()
  "docstring"
  (interactive)
  (java-ide--add-line
   (if (and meghanada--server-process (process-live-p meghanada--server-process))
       (let ((sym (meghanada--what-symbol))
             (buf (buffer-file-name))
             (line (meghanada--what-line))
             (col (meghanada--what-column)))

         (concat
          "stop at "
          (first (meghanada--send-request-sync "ti"
                                               buf
                                               line
                                               col
                                               (format "\"%s\"" sym)))
          ":"
          line))
     (error "client connection not established"))
   "~/.jdbrc")
  (bm-bookmark-add))

(defun java-ide-debug-class ()
  "docstring"
  (interactive )
  (meghanada-debug-junit-class)
  (ignore-errors (realgud:jdb "jdb -attach 6006"))
  (sleep-for 0.2)
  (realgud-cmdbuf-toggle-in-debugger?)

  (realgud-send-command "run"))

(defun java-ide-debug-test ()
  "docstring"
  (interactive )
  (meghanada-debug-junit-test-case)
  (ignore-errors (realgud:jdb "jdb -attach 6006"))
  (sleep-for 0.2)
  (realgud-cmdbuf-toggle-in-debugger?)

  (realgud-send-command "run"))



(with-current-buffer "CachingProvider.java"
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (meghanada--send-request "jd" #'meghanada--jump-callback
                               (buffer-file-name)
                               10
                               0
                               (format "\"%s\"" "java.lang.String"))
    (message "client connection not established")))

(provide 'java-ide)
;;; java-ide.el ends here
