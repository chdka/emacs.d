;;; chdka-helper-functions.el --- -*- lexical-binding: t -*-

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

;; This file contains some functions which are usefull throughout my
;; emacs configuration

;;; Code:

(defun chdka-hf-nowrap-and-truncate-line ()
  "Disable visual line wrap and truncate line"
  (interactive)
  (visual-line-mode -1)
  (toggle-truncate-lines 1))


(defun chdka-hf-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged.
ref: https://github.com/sprig/org-capture-extension"
  (concat
  (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )


(defun chdka-hf-generate-toc ()
  " Generate a TOC for a file for quick navigation, mostly is use this in markdown files
ref: https://gitlab.com/skybert/my-little-friends/-/blob/master/emacs/.emacs"
  (interactive)
  (occur "^#+ [0-9a-zA-Z]+")
  )

;;;; Provide
(provide 'chdka-helper-functions)
;;; chdka-helper-functions.el ends here
