;;; init-ivy.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1")) ((ivy))

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

;; This packag installs ivy with personal settings

;;; Code:
;; =====
(message "init-ivy.el @Ivy - completion framework :@behaviour:..")

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

(provide 'init-ivy)
;;; init-ivy.el ends here
