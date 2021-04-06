;;; init-deft.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1")) ((deft)) ((chdka-deft-plus))

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

;; This packag installs deft with personal settings

;;; Code:
;; =====
(message "init-deft.el @Deft..")

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


(provide 'init-deft)
;;; init-deft.el ends here
