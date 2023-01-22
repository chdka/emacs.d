;;; chdka-deft-plus.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2021  Christian Dijkstra <chdka@public-files.de>

;; Author: Christian Dijkstra <chdka@public-files.de>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (deft))

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

;; This package enhances deft, currently it provides better titles.
;;
;; it is based on:
;;  https://jingsi.space/post/2017/04/05/organizing-a-complex-directory-for-emacs-org-mode-and-deft/

;;; Code:

(defun chdka-deft/strip-quotes (str)
  (cond ((string-match "\"\\(.+\\)\"" str) (match-string 1 str))
        ((string-match "'\\(.+\\)'" str) (match-string 1 str))
        (t str)))

(defun chdka-deft/parse-title-from-front-matter-data (str)
  (if (string-match "^title: \\(.+\\)" str)
      (let* ((title-text (chdka-deft/strip-quotes (match-string 1 str)))
             (is-draft (string-match "^draft: true" str)))
        (concat (if is-draft "[DRAFT] " "") title-text))))

(defun chdka-deft/deft-file-relative-directory (filename)
  (file-name-directory (file-relative-name filename deft-directory)))

(defun chdka-deft/title-prefix-from-file-name (filename)
  (let ((reldir (chdka-deft/deft-file-relative-directory filename)))
    (if reldir
        (concat (directory-file-name reldir) " > "))))

(defun chdka-deft/parse-title-with-directory-prepended (orig &rest args)
  (let ((str (nth 1 args))
        (filename (car args)))
    (concat
      (chdka-deft/title-prefix-from-file-name filename)
      (let ((nondir (file-name-nondirectory filename)))
        (if (or (string-prefix-p "README" nondir)
                (string-suffix-p ".txt" filename))
            nondir
          (if (string-prefix-p "---\n" str)
              (chdka-deft/parse-title-from-front-matter-data
               (car (split-string (substring str 4) "\n---\n")))
            (apply orig args)))))))


(provide 'chdka-deft-plus)
;;; chdka-deft-plus.el ends here
