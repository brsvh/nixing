;;; my-programming-nix.el --- Porgramming with Nix -*- lexical-binding: t -*-

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

;; This file has enhanced my Nix programming experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'apheleia-formatters)
  (require 'nix-ts-mode))



;;;
;; Major modes:

(setup nix-ts-mode
  (:with-mode nix-ts-mode
    (:file-match
     "\\.nix\\'")))



;;;
;; Format:

(setup apheleia
  (:when-loaded
    ;; REVIEW Remove this after radian-software/apheleia#300 is merged.
    (:set (append apheleia-mode-alist) '(nix-ts-mode . nixfmt))))



(provide 'my-programming-nix)
;;; my-programming-nix.el ends here
