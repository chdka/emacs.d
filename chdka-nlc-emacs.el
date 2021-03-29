;;; chdka-nlc-emacs.el --- -*- lexical-binding: t -*-

;;; License:
;; Copyright (C) 2021
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
;; It loads my selected packages using straight.el and use-package

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

(defconst chdka-emacs--env-home-config-emacs (expand-file-name (getenv "CHDKA_HOME_CONFIG_EMACS"))
   "This path points to the location where my emacs configuration is stored [DEPRECATED]")

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

(setq-default custom-file (expand-file-name "chdka-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))


;;;; Assign a backup and auto-save folder and set the housekeeping:
(message "chdka-nlc-emacs.el @Assign a backup and auto-save folder..")

(defconst chdka-emacs-const-backup-rootdir
          (expand-file-name "/emacs/backup/" chdka-emacs--env-home-cache )
  "Personal Emacs backup root.")

(if (not (file-exists-p chdka-emacs-const-backup-rootdir))
    (make-directory chdka-emacs-const-backup-rootdir t))
(setq-default backup-directory-alist
              `((".*" . ,chdka-emacs-const-backup-rootdir)))

(defconst chdka-emacs-const-autosave-rootdir
          (expand-file-name "/emacs/auto-save/" chdka-emacs--env-home-cache )
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


;;;; Set the preferred encoding to UTF-8-UNIX when possible:
;; See reference: https://stackoverflow.com/questions/1674481/how-to-configure-gnu-emacs-to-write-unix-or-dos-formatted-files-by-default
(prefer-coding-system 'utf-8-unix)
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))


;;;; Performance issue on MS Windows when pasting unicode text:
;; Sometimes when you paste unicode text in Emacs it responds very slow
;; See reference: https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(when (eq system-type 'windows-nt)
  (setq inhibit-compacting-font-caches t))


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

(defvar chdka-emacs--font-height (if (> (x-display-pixel-width) 1920)
                                     (+ 0 110)
                                   (+ 0 90))
  "This variable holds the font height, which is based on my display width pixelcount")

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

;;; All between these lines vvv move to separate files
;; =====
(message "chdka-emacs.org @Install =org-plus-contrib=..")

;; ----

(straight-use-package 'org-plus-contrib)


;; =====
(message "chdka-emacs.org @Org-mode default settings :@behaviour:..")

;; ----

(setq org-directory chdka-emacs--env-emacs-home-org
      org-catch-invisible-edits 'error           ; prevent editing invisible text
      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      )


;; =====
(message "chdka-emacs.org @TODO keywords and state triggers :@behaviour:..")

;; ----

;; sequence for Final Version Perfected (FVP) variant
;; contains some old sequences (complete words e.g. TODO NEXT ...
;; meaning of my symbols:
;;     .T. : TODO
;;     .I. : INTERRUPT, urgent
;;     .P. : PROGRESS, working on
;;     .N. : NEXT
;;     .W. : WAITING
;;     .D. : DONE
;;     .C. : CANCELLED
;;     >>> : SCHEDULED MEETING :deprecated
;;     >S> : SCHEDULED MEETING
;;     <<< : VISITED MEETING   :deprecated
;;     <V< : VISITED MEETING
;;     >C< : CANCELLED MEETING

(setq org-todo-keywords
      (quote ((sequence ".T.(t!)" ".I.(i!)" ".P.(p@/!)" ".N.(n!)" ".W.(w@/!)" "|" ".D.(d)" ".C.(c)" )
              (sequence ">S>(s@/!)" " >>>(>@/!)" "|" "<V<(v)" ">C<(x)" "<<<" )
              )
             )
      )

(setq org-todo-state-tags-triggers
      (quote ((".T." ("dot") ("waiting") ("meeting"))
              (".I." ("dot") ("waiting") ("urgent" .t) ("meeting"))
              (".P." ("dot") ("waiting") ("progress" .t) ("meeting"))
              (".N." ("dot" . t) ("waiting") ("meeting"))
              (".W." ("dot") ("progress") ("urgent") ("waiting" .t))
              (">>>" ("dot") ("meeting" .t) ("visited" ))
              (done ("waiting") ("hold"))
              (".D."  ("dot") ("waiting") ("meeting"))
              ("<<<"  ("dot") ("meeting") ("visited"  .t))
              )
             )
      ) 


;; =====
(message "chdka-emacs.org @Logging of state changes :@behaviour:..")

;; ----

(setq org-log-into-drawer t)
(setq org-log-done (quote note))
(setq org-log-reschedule (quote note))
(setq org-log-redeadline (quote note))
(setq org-log-delschedule (quote note))
(setq org-log-deldeadline (quote note))
(setq org-log-state-notes-insert-after-drawers nil)

;; Setup log note templates. Add "to [new date]" in reschedule and redeadline
(setq org-log-note-headings '((done        . "CLOSING NOTE %t")
                              (state       . "State %-12s from %-12S %t")
                              (note        . "Note taken on %t")
                              (reschedule  . "Schedule changed on %t: %S -> %s")
                              (delschedule . "Not scheduled, was %S on %t")
                              (redeadline  . "Deadline changed on %t: %S -> %s")
                              (deldeadline . "Removed deadline, was %S on %t")
                              (refile      . "Refiled on %t")
                              )
      )


;; =====
(message "chdka-emacs.org @Define an agenda view :@ui:hook:..")

;; ----

(setq calendar-week-start-day     1       ; monday is 1st day of week
      org-agenda-start-on-weekday nil     ; 
      org-agenda-span             4       ; show 4 days
      org-agenda-start-day      "-0d"
      ;; disable follow mode, use F in agenda view to follow
      org-agenda-start-with-follow-mode nil
      ;; do not show items with active date in global todo list
      ;;  active date are between < and >
      ;;  scheduled, deadline
      org-agenda-todo-ignore-with-date nil)

(setq org-agenda-custom-commands 
      '(("n" "Agenda / .I. / .P. / .N. / .W."
         ((agenda "" nil)
          (todo ".I." nil)
          (todo ".P." nil)
          (todo ".N." nil)
          (todo ".W." nil)
          (todo ".T." nil))
          nil)))

;; disable visual line wrap for agenda buffer only
(add-hook 'org-agenda-mode-hook (lambda ()
                                  (visual-line-mode -1)
                                  (toggle-truncate-lines 1)))


;; =====
(message "chdka-emacs.org @Add files to the agenda :@behaviour:..")

;; ----

(setq org-agenda-files (list (expand-file-name "/GED/GED.org" chdka-emacs--env-emacs-home-org )
                             (expand-file-name "/GED/refile.org" chdka-emacs--env-emacs-home-org )
                             (expand-file-name "/GED/GED-work.org" chdka-emacs--env-emacs-home-org )))


;; =====
(message "chdka-emacs.org @Work related:  Outlook 2 Orgmode :@behaviour:..")

;; ----

(when (not chdka-emacs--const-is-own-device)
   (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
   (add-to-list 'org-agenda-files (expand-file-name "/_ol2orgmode/" chdka-emacs--env-emacs-home-org ))
  )


;; =====
(message "chdka-emacs.org @Activate =org-protocol= :@behaviour:..")

;; ----

(require 'org-protocol)


;; =====
(message "chdka-emacs.org @Configure =org-capture= :@behaviour:..")

;; ----

(setq org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 4))
      )

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm) 


;; =====
(message "chdka-emacs.org @Templates for =org-capture= :@behaviour:..")

;; ----

;;; https://github.com/sprig/org-capture-extension

(defun chdka-emacs--square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat 
  (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )


(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline
                                 (lambda () (expand-file-name "/GED/GED.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
               "* .T. %?\n") ; <-- add to my lisf of todo
              ("i" "interrupt, urgent" entry (file+headline
                                 (lambda () (expand-file-name "/GED/GED.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
               "* .I. %?\n") ; <-- add to my lisf of todo
              ("n" "next" entry (file+headline
                                 (lambda () (expand-file-name "/GED/GED.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
               "* .N. %?\n") ; <-- add to my list of todo, and start immediately
              ("m" "meeting" entry (file+headline
                                    (lambda () (expand-file-name "/GED/GED.org" chdka-emacs--env-emacs-home-org )) "Tijdgebonden")
               "* >>> %?\n %^t") ; <-- add to my list of meetings.
              
              ; https://github.com/sprig/org-capture-extension
              ("p" "protocol" entry (file+headline
                                     (lambda () (expand-file-name "/GED/GED.org" chdka-emacs--env-emacs-home-org )) "Refile")
               "* .T. %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?") ; <-- add from firefox to list
              ("L" "protocol link" entry (file+headline
                                     (lambda () (expand-file-name "/GED/GED.org" chdka-emacs--env-emacs-home-org )) "Refile")
               "* .T. %?%(chdka-emacs--square-brackets-to-round-ones \"%:description\") \n\nCaptured On: %U\n\nAantekeningen: \n")
              )
             )
      )


;; =====
(message "chdka-emacs.org @Org-mode =hooks= :hook:..")

;; ----

(add-hook 'org-mode-hook #'visual-line-mode 1)
(add-hook 'org-mode-hook #'org-indent-mode)

;; removed this hook, it doesn't work correctly with org-roam
;;
;;(require 'org-id)
;;
;;(defun chdka-org-ids-to-headlines-in-file ()
;;  "Add ID properties to all headlines in the current file which do not already have one."
;;  (interactive)
;;  (org-map-entries 'org-id-get-create))
;;
;;(add-hook 'org-mode-hook (lambda ()
;;                           (add-hook 'before-save-hook
;;                                     'chdka-org-ids-to-headlines-in-file nil
;;                                     'local)
;;                           )
;;          )
;;(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)


;; =====
(message "chdka-emacs.org @Org-mode =key mappings= :keymapping:..")

;; ----

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c '") 'org-edit-src-code)
(global-set-key (kbd "C-c i") 'org-id-copy) 


;; =====
(message "chdka-emacs.org @Ivy - completion framework :@behaviour:..")

;; ----

(use-package ivy
  :straight t
  :demand
  :config
  ;; add recent files to the switch-buffer
  ;; current and total # in prompt
  (setq ivy-use-virtual-buffers t
        ivy-count-format "[%d/%d] ")
  
  ;; enable ivy-mode
  (ivy-mode 1)
  )   


;; =====
(message "chdka-emacs.org @Which key :@behaviour:..")

;; ----

(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-mode)
  )


;; =====
(message "chdka-emacs.org @Markdown mode..")

;; ----

(use-package markdown-mode
  :straight t
  :demand t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )


;; =====
(message "chdka-emacs.org @Magit..")

;; ----

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
)


;; =====
(message "chdka-emacs.org @Modus themes :@ui:..")

;; ----

  (use-package modus-themes
    :straight t
    :init
    ;; Add all your customizations prior to loading the themes
    (setq modus-themes-slanted-constructs t
          modus-themes-bold-constructs nil
          modus-themes-intense-hl-line t)

    ;; Load the theme files before enabling a theme
    (modus-themes-load-themes)
    :config
    ;; Load the theme of your choice:
    (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
    :bind ("<f5>" . modus-themes-toggle))



;; =====
(message "chdka-emacs.org @Deft..")

;; ----

  (use-package deft
    :straight (:type git :host github :repo "chdka/deft")
    :bind ("<f8>" . deft)
    :commands (deft)
    :config
     (if chdka-emacs--const-is-own-device
                ; it is my own device
                (setq deft-extensions '("md" "txt" "org")
                      deft-default-extension "md") ; workaround, the default extension is not set properly
              ; it is my work device
              (setq deft-extensions '("md" "org")
                    deft-default-extension "org"))
            (setq deft-directory  (expand-file-name chdka-emacs--env-emacs-home-org)
                  deft-new-file-format "%Y%m%d-%H%M"
                  deft-recursive t
                  )
          (require 'chdka-deft-plus)
          (advice-add 'deft-parse-title :around #'chdka-deft/parse-title-with-directory-prepended)
      )



;; =====
(message "chdka-emacs.org @Elfeed =key-mappings= :keymapping:..")

;; ----

(global-set-key (kbd "<f9>") 'elfeed)


;; =====
(message "chdka-emacs.org @Install elfeed-org..")

;; ----

  (use-package elfeed-org
   :straight (:type git :host github :repo "chdka/elfeed-org")
   :config
     (elfeed-org)
     (setq rmh-elfeed-org-files '()) ; empty the list
     (when (file-exists-p (expand-file-name "76/09-elfeed-76.org" chdka-emacs--env-emacs-home-org))
        (add-to-list 'rmh-elfeed-org-files (expand-file-name "76/09-elfeed-76.org" chdka-emacs--env-emacs-home-org)))
     (when (file-exists-p (expand-file-name "W/09-elfeed-w.org" chdka-emacs--env-emacs-home-org))
        (add-to-list 'rmh-elfeed-org-files (expand-file-name "W/09-elfeed-w.org" chdka-emacs--env-emacs-home-org))))


;; =====
(message "chdka-emacs.org @Install package =elfeed=..")

;; ----

  (use-package elfeed
   :straight t
   :bind (:map elfeed-search-mode-map)
  )

;;; All between these lines ^^^ move to separate files

;;; chdka-nlc-emacs.el ends here
