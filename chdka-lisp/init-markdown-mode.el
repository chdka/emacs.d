;;; init-markdown-mode.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1")) ((markdown-mode))

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

;; This packag installs markdown-mode with personal settings

;;; Code:
;; =====
(message "init-markdown-mode.el @Markdown mode..")

;; ----

(require 'chdka-helper-functions)

(use-package markdown-mode
  :ensure t
  :bind
  (("<f6>" . chdka-hf-generate-toc))
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.\\(?:md\\)\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode 1)
  (setq markdown-regex-yaml-metadata-border "\\(\\.\\{3\\}\\|-\\{3\\}\\)$")
  (setq markdown-wiki-link-alias-first nil
        markdown-enable-wiki-links t
        markdown-wiki-link-search-type 'project)
  :init
  (setq markdown-command "pandoc.exe")
  )
 
(provide 'init-markdown-mode)
;;; init-markdown-mode.el ends here
