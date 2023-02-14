;;; init-org.el --- -*- lexical-binding: t -*-

;;; License:
;; Copyright (C) 2021-2023  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1")) ((org)) ((chdka-helper-functions))

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

;; Except otherwise noted. For example, I derive some functions from
;; Holger Schurig which are licensed under GPLv2.
;; [http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html]

;;; Description:

;; This package installs org with personal settings
;; configures: org-agenda; org-capture; org-protocol

;;; Code:
;;;; Install org
(require 'chdka-helper-functions)

(message "init-org.el @Install =org=..")

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

   ;; in my notes folder, this for better integration in my notes
   (when (file-exists-p (expand-file-name "GED-notes.org" chdka-emacs--env-emacs-home-notes))
     (add-to-list 'org-agenda-files (expand-file-name "GED-notes.org" chdka-emacs--env-emacs-home-notes)))
   (when (file-exists-p (expand-file-name "refile-notes.org" chdka-emacs--env-emacs-home-notes))
     (add-to-list 'org-agenda-files (expand-file-name "refile-notes.org" chdka-emacs--env-emacs-home-notes)))

   
   (setq chdka--writable-targets org-agenda-files)

   ;; my work outlook 2 org mode files, these files are read-only and thus can not used as a refile target
   (when (file-exists-p (expand-file-name "WA/Ol2Om-agda.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "WA/Ol2Om-agda.org" chdka-emacs--env-emacs-home-org)))
   (when (file-exists-p (expand-file-name "WA/Ol2Om-tsks.org" chdka-emacs--env-emacs-home-org))
     (add-to-list 'org-agenda-files (expand-file-name "WA/Ol2Om-tsks.org" chdka-emacs--env-emacs-home-org)))

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

;;; Provide
(provide 'init-org)
;;; init-org.el ends here
