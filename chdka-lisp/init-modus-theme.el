;;; init-modus-theme.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1")) ((modus-theme))

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

;; This packag installs modus theme with personal settings

;;; Code:
;; =====
(message "init-modue-theme.el @Modus themes :@ui:..")

;; ----

(use-package modus-themes
  :ensure t
  :demand t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-intense-hl-line t)
  ;; Load the theme of your choice:
  (load-theme 'modus-operandi) ;; OR (modus-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(provide 'init-modus-theme)
;;; init-modus-theme.el ends here
