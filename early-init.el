;;; early-init.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; Package-requires: ((emacs "27.1"))

;; This file is part of my Emacs configuration

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Description: 

;; This file is loaded before the package system and GUI is initialized.
;; Here I customize the appearance of my frames.

;;; Code:

;; prevent package.el loading packages, this is handled via straight.el
;; and use-package
(setq package-enable-at-startup nil)

;; bootstrap straight.el
(defvar bootstrap-version)
  
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)


;; Initialize GUI elements
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(setq inhibit-splash-screen t)


;;; early-init.el ends here
