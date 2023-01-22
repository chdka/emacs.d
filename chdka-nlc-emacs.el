;;; chdka-nlc-emacs.el --- -*- lexical-binding: t -*-

;;; License:
;; Copyright (C) 2021-2023
;; Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; Package-requires: ((emacs "27.1"))

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

;; This file defines my non literate configuration.
;; It loads my selected packages use-package


;;; Code:
;;;; Set file and path constants based on environment variables:
(message "chdka-nlc-emacs.el @Set file and path constants based on environment variables..")

(defconst chdka-emacs--env-device (getenv "CHDKA_DEVICE")
   "Indicates if this device is my own device or work")

(defconst chdka-emacs--env-emacs-home-beorg (expand-file-name (getenv "CHDKA_APP_EMACS_HOME_BEORG"))
   "This the path to my files used via the iOS app Beorg")

(defconst chdka-emacs--env-emacs-home-org (expand-file-name (getenv "CHDKA_APP_EMACS_HOME_ORG"))
   "This is the path to my folder where i store my org files")

(defconst chdka-emacs--env-home-cache (expand-file-name (getenv "CHDKA_HOME_CACHE"))
   "This path is my equivalent of XDG_CACHE_HOME")

(defconst chdka-emacs--env-home-config (expand-file-name (getenv "CHDKA_HOME_CONFIG"))
   "This path is my equivalent of XDG_CONFIG_HOME")

(defconst chdka-emacs--env-home-data (expand-file-name (getenv "CHDKA_HOME_DATA"))
   "This path points to the location where my data is stored e.g. FireFox profile")

(defconst chdka-emacs--env-home-logs (expand-file-name (getenv "CHDKA_HOME_LOGS"))
   "This path points to the location where logs are stored of my personal scripts")

(defconst chdka-emacs--env-home-projects (expand-file-name (getenv "CHDKA_HOME_PROJECTS"))
   "This path points to the location where my programming projects are stored")

(defconst chdka-emacs--env-home-user (expand-file-name (getenv "CHDKA_HOME_USER"))
   "This path points to 'HOME' [DEPRECATED]")

(defconst chdka-emacs--env-xdg-cache-home  (expand-file-name (getenv "XDG_CACHE_HOME")))
(defconst chdka-emacs--env-xdg-config-home (expand-file-name (getenv "XDG_CONFIG_HOME")))
(defconst chdka-emacs--env-xdg-data-home   (expand-file-name (getenv "XDG_DATA_HOME")))
(defconst chdka-emacs--env-emacs-server-file (expand-file-name (getenv "EMACS_SERVER_FILE")))


;;;; Set constant if device is my *OWN*:
;; This is used for conditional settings
(message "chdka-nlc-emacs.el @Set constant if device is my *OWN*..")

(defconst chdka-emacs--const-is-own-device
  (string-suffix-p "OWN" chdka-emacs--env-device t)
  "This pseudo constant indicates if my current configuration is running on my personal device,
or if it is running on my work device")


;;;; Set some behaviour settings:
(message "chdka-nlc-emacs.el @Set some behaviour settings..")

;; fset is used vs defalias, fset is global where defalias registered to the loaded file
;; lisp file
;; - [https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Functions.html]
;; - [https://www.reddit.com/r/emacs/comments/4fzgdn/what_difference_there_is_between_defalias_and_fset/]
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default confirm-kill-emacs 'yes-or-no-p)


;;;; Reading and writing files
(message "chdka-nlc-emacs.el @Reading and writing files..")

(setq large-file-warning-threshold 150000000)

(defconst chdka-emacs-const-backup-rootdir
          (expand-file-name "emacs/backup/" chdka-emacs--env-home-cache )
  "Personal Emacs backup root.")

(if (not (file-exists-p chdka-emacs-const-backup-rootdir))
    (make-directory chdka-emacs-const-backup-rootdir t))
(setq-default backup-directory-alist
              `((".*" . ,chdka-emacs-const-backup-rootdir)))

(defconst chdka-emacs-const-autosave-rootdir
          (expand-file-name "emacs/auto-save/" chdka-emacs--env-home-cache )
  "Personal Emacs auto-save root.")

(if (not (file-exists-p chdka-emacs-const-autosave-rootdir))
    (make-directory chdka-emacs-const-autosave-rootdir t))
(setq-default auto-save-file-name-transforms 
              `((".*" ,(file-name-as-directory chdka-emacs-const-autosave-rootdir) t)))


(message "chdka-nlc-emacs.el @..housekeeping for backup and auto-save")

(setq-default make-backup-files t    ; backup of a file the first time it is saved
              backup-by-copying t
              version-control t      ; version numbers for backup files
              delete-old-versions t  ; delete old versions silently
              delete-by-moving-to-trash t
              kept-old-versions 6    ; oldest version to keep
              kept-new-versions 9    ; newest version to keep
              )

(setq-default auto-save-default t    ; auto-save every buffer that visits a file
              auto-save-timeout 20   ; number of seconds idle time before auto-save
              auto-save-interval 200 ; number of keystrokes between auto-saves
              )


;;;; Set some defaults for the buffer:
(message "chdka-nlc-emacs.el @Set defaults for the buffer")

(global-auto-revert-mode 1)                   ; auto update buffer if a file changes
(add-hook 'dired-mode-hook 'auto-revert-mode) ; auto refresh dired when file changes
(setq-default line-number-mode t
              column-number-mode t
              scroll-conservatively 1000
              ring-bell-function 'ignore
              visible-bell 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Disable bidirectional text to prevent "trojan source" exploits
;;  see https://www.trojansource.codes/
(setf (default-value 'bidi-display-reordering) nil)


;;;; Set the preferred encoding to UTF-8-UNIX when possible:
;; See reference: https://stackoverflow.com/questions/1674481/how-to-configure-gnu-emacs-to-write-unix-or-dos-formatted-files-by-default
(prefer-coding-system 'utf-8-unix)
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))


;;;; Set tabs in the buffer:
(setq-default indent-tabs-mode nil
              tab-with 4
              require-final-newline t)


;;;; Enable syntax higlighting for all buffers, show matching parentheses:
(global-font-lock-mode t)
(setq show-paren-delay 0)
(show-paren-mode 1)


;;;; Set the base fonts:
(message "chdka-nlc-emacs.el @Set the base fonts..")

(defvar chdka-emacs--font-height (if (> (x-display-pixel-height) 1080)
                                     (+ 0 100)
                                   (+ 0 90))
   "This variable holds the font height, which is based on my display width and pixelcount")

;; Main typeface
(set-face-attribute 'default nil :family "Fantasque Sans Mono" :height chdka-emacs--font-height)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Georgia" :height 1.0)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "Fantasque Sans Mono" :height 1.0)


;;;; Set some frame / window defaults:
(message "chdka-nlc-emacs.el @Set some frame / window defaults..")

(setq-default frame-title-format
              '((:eval (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name))
                         "%b"))))
(blink-cursor-mode 1)
(global-hl-line-mode t)

;;; Load all the installed packages
(message "chdka-nlc-emacs.el @Load all the installed packages..")

(require 'init-modus-theme)
(require 'init-ivy)
(require 'init-which-key)
(require 'init-org)
(require 'init-deft)
(require 'init-markdown-mode)
(require 'init-elfeed)
(require 'init-magit)
(require 'init-lsp-mode)
;; (require 'init-ffip)

;;; All between these lines vvv move to separate files
;;; All between these lines ^^^ move to separate files


;;; chdka-nlc-emacs.el ends here
