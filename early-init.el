;;; early-init.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021
;; Christian Dijkstra <chdka@public-files.de>

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

;; STARTUP: set some debugging and initialize some variables
;; ---------------------------------------------------------
(message "early-init.el @ STARTUP..")

(setq debug-on-error t
      debug-on-quit t)

;; Startup timing
(defvar chdka-early-init--emacs-start-time (current-time)
  "Time when Emacs was started")


;; PACKAGE CONFIGURATION: bootstrap straight.el
;; --------------------------------------------
(message "early-init.el @ PACKAGE CONFIGURATION straight.el...")

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


;; GUI: Initialize some GUI elements
;; ---------------------------------
(message "early-init.el @ GUI..")

;; Initialize GUI elements
(add-to-list 'initial-frame-alist '(fullscreen . maximised))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)



;; END: 
;; ----
(message "early-init.el @ END.\n")

;;; early-init.el ends here
