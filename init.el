;;; init.el --- -*- lexical-binding: t -*-

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

;; This file sets some basic settings.

;;; Code:

;; STARTUP: set some debugging and variables
;; -----------------------------------------
(message "init.el @ STARTUP..")

(if (version< emacs-version "27.1")
    (progn (message "init.el @ STARTUP - load early-init.el (emacs-version < 27.1)...\n")
	   (if (load (expand-file-name "early-init.el" user-emacs-directory) 'missing-ok  )
	       (message "\ninit.el @ STARTUP - load early-init.el succeeded\n")
	     (message "init.el @ STARTUP - load early-init.el failed\n")))
  (message "init.el @ STARTUP - early-init.el already loaded..."))


;; PATH DEFAULTS: set some default loading paths and current dir
;; -------------------------------------------------------------
(message "init.el @ PATH DEFAULTS..")

;; Set to a sane startup folder
;; (more important when emacsserver is used)
(cd (expand-file-name "~"))

;; Pull in ./chdka-lisp/*
(add-to-list 'load-path (expand-file-name "chdka-lisp" user-emacs-directory))


;; MAIN: start the main configuration
;; ----------------------------------
(message "init.el @ MAIN..")


;; FINALIZE: turn debugging off
;; ----------------------------
(message "init.el @ FINALIZE..")

;; Startup timing results
(message "\nStart up time %.2fs\n"
	 (float-time (time-subtract (current-time) chdka-early-init--emacs-start-time)))

(setq debug-on-error nil
      debug-on-quit nil)


;; END:
;; ----
(message "init.el @ END.\n")

;;; init.el ends here