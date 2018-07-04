;;; local.el ---                                     -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;
;;; Code:

(setq lsp-java--workspace-folders (list "/home/kyoncho/Sources/tick42-gds/"
                                        "/home/kyoncho/Sources/cm/java-server-backend/"
                                        "/home/kyoncho/Sources/cm/java-storage-common/"
                                        "/home/kyoncho/Sources/cm/java-storage-file/"
                                        "/home/kyoncho/Sources/cm/java-server-app/"
                                        ;; "/home/kyoncho/Sources/cm-workbench/"
                                        "/home/kyoncho/Sources/cm/java-server-core/"
                                        "/home/kyoncho/Sources/cm/java-client-hazel/"
                                        "/home/kyoncho/Sources/cm/java-client-factory/"
                                        "/home/kyoncho/Sources/java-aim-gen/"
                                        "/home/kyoncho/Sources/clj-gateway/app-config/"))

(provide 'local)
;;; local.el ends here
