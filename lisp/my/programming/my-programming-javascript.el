;;; my-programming-javascript.el --- Programming with JavasCript -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/brsvh/my-emacs
;; Version: 0.1.50

;; This file is part of my-emacs.

;; my-emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; my-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file has enhanced my JavaScript operating and programming
;; experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'js)
  (require 'eglot))



;;;
;; Major modes:

;; REVIEW Is the default major mode good?
(setup js
  (:autoload js-mode js-ts-mode))



;;;
;; LSP:

(setup eglot
  (:autoload eglot-ensure))

;; Activate `eglot' for all JavaScript mode.
(setup js
  (:with-mode js-ts-mode
    (:hook #'eglot-ensure))
  (:with-mode js-mode
    (:hook #'eglot-ensure)))



(provide 'my-programming-javascript)
;;; my-programming-javascript.el ends here
