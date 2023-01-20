;;; init.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023
;; Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; Package-requires: ((emacs > "27.1"))

;; This file is part of my personal Emacs configuration

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


;; PACKAGEMANAGEMENT: set repos and ensure use-package is installed
;; ----------------------------------------------------------------
(message "init.el @ PACKAGEMANAGEMENT..")

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu". "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;(setq package-gnupghome-dir (expand-file-name "elpa/gnupg" user-emacs-directory))

;; ensure use-package is installed
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

;; customization in its own file
(setq-default custom-file (expand-file-name "chdka-custom.el" user-emacs-directory))
(load custom-file 'noerror)


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

(setq chdka-init--use-lc nil) ; when t then load my literate config

(when chdka-init--use-lc
  ; use literate config
  (if (not (require 'chdka-literate-config nil t))
      (message "init.el @ MAIN - 'chdka-literate-config' not found")
    (message "init.el @ MAIN - load chdka-literate-config..\n")
    (chdka-lc-load-config-file "chdka-emacs.org"))
  )

(when (not chdka-init--use-lc)
  ; use the non literate config
  (if (load (expand-file-name "chdka-nlc-emacs.el" user-emacs-directory) 'missing-ok )
      (message "\ninit.el @ MAIN - load chdka-nlc-emacs.el succeeded\n")
    (message "init.el @ MAIN - load chdka-nlc-emacs.el failed\n"))
  )
    

;; FINALIZE:
;; ----------------------------
(message "init.el @ FINALIZE..")

;; Startup timing results
(message "\nStart up time %.2fs\n"
 (float-time (time-subtract after-init-time before-init-time)))

;; END:
;; ----
(message "init.el @ END.\n")

;;; init.el ends here
