;;; init-elfeed.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1")) ((elfeed)) ((elfeed-org))

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

;; This packag installs elfeed and elfeed-org with personal settings

;;; Code:
;; =====
(message "init-elfeed.el @Install elfeed-org..")

;; ----

(use-package elfeed-org
  :ensure t
  :config
     (elfeed-org)
     (setq rmh-elfeed-org-files '()) ; empty the list
     (when (file-exists-p (expand-file-name "CP/09-elfeed-76.org" chdka-emacs--env-emacs-home-org))
        (add-to-list 'rmh-elfeed-org-files (expand-file-name "CP/09-elfeed-76.org" chdka-emacs--env-emacs-home-org)))
     (when (file-exists-p (expand-file-name "CN/09-elfeed-w.org" chdka-emacs--env-emacs-home-org))
       (add-to-list 'rmh-elfeed-org-files (expand-file-name "CN/09-elfeed-w.org" chdka-emacs--env-emacs-home-org)))
   :bind ("<f9>" . elfeed)
     )


;; =====
(message "init-elfeed.el @Install package =elfeed=..")

;; ----

  (use-package elfeed
   :ensure t
   :bind (:map elfeed-search-mode-map)
  )


(provide 'init-elfeed)
;;; init-elfeed.el ends here
