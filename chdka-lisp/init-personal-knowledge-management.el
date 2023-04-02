;;; init-personal-knowledge-management.el -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of Emacs

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

;;  This configuration incorporates my settings for my personal
;;  knowledge system
;;  Currently this consists of the following packages:
;;  - org    - for agenda and capture todo's
;;  - denote - for quickly writing uniform files and filenames
;;  - deft   - for navigating reasonable quit through my notes


;;; Keyword for the use-package are
;;  :disabled                             ; the use-package statement is not executed
;;                                        ; makes it possible to temporary disable the package
;;  :if                                   ; conditional loading of the package
;;  :demand                               ; force the loading of the package
;;  :commands
;;  :mode
;;  :interpreter
;;  :magic
;;  :hook                                 ; define hooks 
;;  :requires
;;  :load-path
;;  :after
;;  :defines                              ; create dummy variables
;;  :functions                            ; and functions
;;  :custom                               ; customization of an package
;;  :custom-face                          ; define the font face
;; ;;; :defer t                           ; postpone the loading of the package
;;                                        ; with straight.el this is not needed
;;  :bind                                 ; define keybindings
;;  :init                                 ; this sections is executed before loading
;;                                        ; of the package
;;  :config                               ; this section is executed after loading
;;                                        ; the package
;;  :diminish
;;  :delight
;;  :no-require t
;;  :preface
;;  :pin
;;  :ensure-system-package

;;; see for all keywords
;;  https://github.com/jwiegley/use-package
;;  and others

;; -----------------------------------------------------------------------------
;; =============================================================================

;;; Code:

(message "    init-personal-knowledge-management.el - init-org..")

(require 'chdka-helper-functions)

(message "    init-personal-knowledge-management.el - install/config org-mode..")

(use-package org
  :ensure t
  :pin org
  :init
  (add-hook 'org-mode-hook #'visual-line-mode 1)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-agenda-mode-hook 'chdka-hf-nowrap-and-truncate-line)
  :bind 
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)
   ("C-c '" . org-edit-src-code)
   ("C-c i" . org-id-copy))  
  :config
  (setq org-directory chdka-emacs--env-emacs-home-org
         org-catch-invisible-edits 'error           ; prevent editing invisible text
         org-use-fast-todo-selection t
         org-treat-S-cursor-todo-selection-as-state-change nil)

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
  ;;     >>> : SCHEDULED MEETING :deprecated, because these are also used in radio targets
  ;;     >S> : SCHEDULED MEETING
  ;;     <<< : VISITED MEETING   :deprecated, because these are also used in radio targets
  ;;     <V< : VISITED MEETING
  ;;     >C< : CANCELLED MEETING
  (setq org-todo-keywords
        (quote ((sequence ".T.(t!)" ".I.(i!)" ".P.(p@/!)" ".N.(n!)" ".W.(w@/!)" "|" ".D.(d)" ".C.(c)" )
                (sequence ">S>(s@/!)" " >>>(>@/!)" "|" "<V<(v)" ">C<(x)" "<<<" ))))

  (setq org-todo-state-tags-triggers
        (quote ((".T." ("dot") ("waiting") ("meeting"))
                (".I." ("dot") ("waiting") ("urgent" .t) ("meeting"))
                (".P." ("dot") ("waiting") ("progress" .t) ("meeting"))
                (".N." ("dot" . t) ("waiting") ("meeting"))
                (".W." ("dot") ("progress") ("urgent") ("waiting" .t))
                (">>>" ("dot") ("meeting" .t) ("visited" ))
                (done ("waiting") ("hold"))
                (".D."  ("dot") ("waiting") ("meeting"))
                ("<<<"  ("dot") ("meeting") ("visited"  .t)))))

  ;;;; Init the logging behaviour:
  (setq org-log-into-drawer t
        org-log-done (quote note)
        org-log-reschedule (quote note)
        org-log-redeadline (quote note)
        org-log-delschedule (quote note)
        org-log-deldeadline (quote note)
        org-log-state-notes-insert-after-drawers nil)

  ;; Setup log note templates. Add "to [new date]" in reschedule and redeadline
  (setq org-log-note-headings '((done        . "CLOSING NOTE %t")
                                (state       . "State %-12s from %-12S %t")
                                (note        . "Note taken on %t")
                                (reschedule  . "Schedule changed on %t: %S -> %s")
                                (delschedule . "Not scheduled, was %S on %t")
                                (redeadline  . "Deadline changed on %t: %S -> %s")
                                (deldeadline . "Removed deadline, was %S on %t")
                                (refile      . "Refiled on %t")))

  ;;;; Define an agenda view:
  (setq calendar-week-start-day     1       ; monday is 1st day of week
        org-agenda-start-on-weekday nil     ;
        org-agenda-span             4       ; show 4 days
        org-agenda-start-day      "-0d"
        org-agenda-start-with-follow-mode nil
        org-agenda-todo-ignore-with-date nil
        org-agenda-sort-notime-is-late nil)

  (setq org-agenda-custom-commands
        '(("n" "Agenda / .I. / .P. / .N. / .W."
           ((agenda "" nil)
            (todo ".I." nil)
            (todo ".P." nil)
            (todo ".N." nil)
            (todo ".W." nil)
            (todo ".T." nil))
            nil)))

   ;;;; Add my files to the agenda:
   (setq org-agenda-files '()) ; create an empty list
   ;; my personal agenda files
   (when (file-exists-p (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org)))
   (when (file-exists-p (expand-file-name "CA/refile.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "CA/refile.org" chdka-emacs--env-emacs-home-org)))

   ;; my work agenda files
   (when (file-exists-p (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org)))
   (when (file-exists-p (expand-file-name "WA/refile-wrk.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "WA/refile-wrk.org" chdka-emacs--env-emacs-home-org)))

   (setq chdka--writable-targets org-agenda-files)

   ;; my work outlook 2 org mode files, these files are read-only and thus can not used as a refile target
   (when (file-exists-p (expand-file-name "WA/Ol2Om-agda.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "WA/Ol2Om-agda.org" chdka-emacs--env-emacs-home-org)))
   (when (file-exists-p (expand-file-name "WA/Ol2Om-tsks.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "WA/Ol2Om-tsks.org" chdka-emacs--env-emacs-home-org)))


   ;; Auto refresh org-agenda, my agenda org files getting refreshed from outside emacs.
   ;; to reflect changes in the org-agenda view a 'cron-like job' is created
   ;; https://emacs.stackexchange.com/questions/16326/how-to-rebuild-agenda-buffers-when-saving-an-org-mode-buffer
   (defun chdka-org-redo-all-agenda-buffers ()
     (interactive)
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
         (when (derived-mode-p 'org-agenda-mode)
           (org-agenda-redo t)))))
   (run-with-timer 0 (* 6 60) 'chdka-org-redo-all-agenda-buffers)

   ;; end of org-mode use-package
   )

;;;; Initialize org-protocol:
(message "    init-personal-knowledge-management.el - install/config org-protocol..")

(use-package org-protocol
  :requires org
  :pin org
  :config
  (setq org-refile-targets '((nil :maxlevel . 4)
                             (chdka--writable-targets :maxlevel . 4)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  (if chdka-emacs--const-is-own-device
      (setq org-capture-templates
            (quote (("t" "todo" entry (file+headline
                                       (lambda () (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
                     "* .T. %?\n") ; <-- add to my lisf of todo
                    ("i" "interrupt, urgent" entry (file+headline
                                                    (lambda () (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
                     "* .I. %?\n") ; <-- add to my lisf of todo
                    ("n" "next" entry (file+headline
                                       (lambda () (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
                     "* .N. %?\n") ; <-- add to my list of todo, and start immediately
                    ("m" "meeting" entry (file+headline
                                          (lambda () (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org )) "Tijdgebonden")
                     "* >>> %?\n %^t") ; <-- add to my list of meetings.

                                        ; https://github.com/sprig/org-capture-extension
                    ("p" "protocol" entry (file+headline
                                           (lambda () (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org )) "Refile")
                     "* .T. %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?") ; <-- add from firefox to list
                    ("L" "protocol link" entry (file+headline
                                                (lambda () (expand-file-name "CA/GED.org" chdka-emacs--env-emacs-home-org )) "Refile")
                     "* .P. %?%(chdka-hf-square-brackets-to-round-ones \"%:description\") \n\nCaptured On: %U\n\nAantekeningen: \n"))))

    (setq org-capture-templates
          (quote (("t" "todo" entry (file+headline
                                     (lambda () (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
                   "* .T. %?\n %a") ; <-- add to my lisf of todo
                  ("i" "interrupt, urgent" entry (file+headline
                                                  (lambda () (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
                   "* .I. %?\n") ; <-- add to my lisf of todo
                  ("n" "next" entry (file+headline
                                     (lambda () (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org )) "Niet Tijdgebonden")
                   "* .N. %?\n") ; <-- add to my list of todo, and start immediately
                  ("m" "meeting" entry (file+headline
                                        (lambda () (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org )) "Tijdgebonden")
                   "* >>> %?\n %^t") ; <-- add to my list of meetings.

                                        ; https://github.com/sprig/org-capture-extension
                  ("p" "protocol" entry (file+headline
                                         (lambda () (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org )) "Refile")
                   "* .T. %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?") ; <-- add from firefox to list
                  ("L" "protocol link" entry (file+headline
                                              (lambda () (expand-file-name "WA/GED-wrk.org" chdka-emacs--env-emacs-home-org )) "Refile")
                   "* .P. %?%(chdka-hf-square-brackets-to-round-ones \"%:description\") \n\nCaptured On: %U\n\nAantekeningen: \n")))))

)


;; -----------------------------------------------------------------------------
(message "    init-personal-knowledge-management.el - install deft..")

(use-package deft
  :ensure t
  :bind
  (("<f7>"   . deft)
   ("S-<f7>" . deft-refresh)) ; after creating a note via denote a refresh is needed
  :commands
  (deft)
  :config
  (if chdka-emacs--const-is-own-device
                                        ; it is my own device
      (setq deft-extensions '("md" "txt" "org")
            deft-default-extension "md"); workaround, the default extension is not set properly
                                        ; it is my work device
    (setq deft-extensions '("md" "org")
          deft-default-extension "md"))
  (setq deft-directory  (expand-file-name chdka-emacs--env-emacs-home-notes)
        deft-new-file-format chdka-emacs--datetime-format
        deft-recursive t
        deft-auto-save-interval 0
        deft-file-limit nil)
  (setq deft-recursive-ignore-dir-regexp
        (concat "\\(?:"
                "\\."        ; current folder
                "\\|\\.\\."  ; up one level
                "\\)$"))
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                "\\|^#\\+[[:lower:]_]+:.*$" ;; org-mode metadata lowercase
                ;; "\\|^---[\n\t]\\(.*[\n\t]\\)+\\.\\.\\.$" ;; deactivated due to performance impact
                ;; "\\|^\\:PROPERTIES\\:[\n\t]\\(.*[\n\t]\\)+\\:END\\:$" ;; deactivated due to performance impact
                "\\)"))
  
  (require 'chdka-deft-plus)
  (advice-add 'deft-parse-title :around #'chdka-deft/parse-title-with-directory-prepended))


;;------------------------------------------------------------------------------
(message "    init-personal-knowledge-management.el - install denote..")

(use-package denote
  :ensure t
  :bind
  (("<f8>" . denote)
   ("M-<f8>" . denote-add-front-matter)
   ("C-<f8>" . denote-rename-file-using-front-matter))
  :config
  (setq denote-id-format chdka-emacs--datetime-format
        denote-id-regexp chdka-emacs--datetime-regexp
        denote-yaml-front-matter (concat "---\n"
                                         "title:      %s\n"
                                         "date:       %s\n"
                                         "tags:       %s\n"
                                         "identifier: %S\n"
                                         "...\n\n"
                                         )
        denote-directory  (expand-file-name "notes" chdka-emacs--env-emacs-home-notes)
        denote-file-type 'markdown-yaml))



;; -----------------------------------------------------------------------------
;; =============================================================================

(provide 'init-personal-knowledge-management)
;;; init-personal-knowledge-management.el ends here
