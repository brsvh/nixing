;;; my-programming-emacs-lisp.el --- Porgramming with Emacs Lisp -*- lexical-binding: t -*-

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

;; This file has enhanced my Emacs Lisp programming experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'parinfer-rust-mode))



;;;
;; Diagnostics:

(setup elisp-mode
  (:with-hook emacs-lisp-mode-hook
    (:hook #'flymake-mode)))



;;;
;; Expansion:

(setup elisp-mode
  (:snoc popper-reference-buffers
         "\\*Pp Macroexpand Output\\*")
  (:with-map emacs-lisp-mode-map
    (:keymap-set "C-c C-v" #'pp-macroexpand-last-sexp)))



;;;
;; Parens editing:

(setup elisp-mode
  (:with-hook emacs-lisp-mode-hook
    (:hook #'parinfer-rust-mode)))



(provide 'my-programming-emacs-lisp)
;;; my-programming-emacs-lisp.el ends here
