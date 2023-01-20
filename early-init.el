;;; early-init.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023
;; Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; Package-requires: ((emacs > "27.1"))

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

;;; STARTUP: initialize some variables
;;  ----------------------------------
(message "early-init.el @ STARTUP..")

;; Garbage collection
;; Increase the GC for faster startup
(setq gc-cons-threshold (* 50 1000 1000))


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

;; https://www.reddit.com/r/emacs/comments/8v4v7o/why_is_emacs_so_slow_on_windows/
;; sometime with pasting unicode text emacs responds very slow.
;; https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(setq inhibit-compacting-font-caches t)


;; END: 
;; ----
(message "early-init.el @ END.\n")

;;; early-init.el ends here
