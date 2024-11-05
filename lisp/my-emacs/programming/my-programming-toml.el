;;; my-programming-toml.el --- Editing with toml -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1") (my-core "0.2.0"))
;; URL: https://github.com/brsvh/shelf
;; Version: 0.2.0

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

;; This file has enhanced my toml editing experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'toml-ts-mode))



;;;
;; Major modes:

(setup toml-ts-mode
  (:with-mode toml-ts-mode
    (:file-match
     "\\.toml\\'")))



(provide 'my-programming-toml)
;;; my-programming-toml.el ends here
