;;; init-lsp-mode.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Christian Dijkstra <chdka@public-files.de>

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

;;  This is add the lsp-mode to my emacs config

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

(message "init-lsp-mode.el - install lsp-mode..")

(use-package lsp-mode
  :disabled t                           ; 
  :straight t                           ; 
  :demand                               ; 
  :after
  :custom                               ; 
  :custom-face                          ; 
  :bind                                 ; 
  :init                                 ; 
                                        ; 
  :config                               ; 
                                        ; 
  )

;; -----------------------------------------------------------------------------
;; =============================================================================

(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
