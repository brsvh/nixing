;;; my-programming.el --- Porgramming support of My Emacs -*- lexical-binding: t -*-

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

;; This file has enhanced my general programming experience, and
;; individually enable support for different programming languages.

;; Supported programming languages:
;;   - Emacs Lisp
;;   - Nix

;;; Code:

(require 'my-core)
(require 'my-programming-cc)
(require 'my-programming-docker)
(require 'my-programming-emacs-lisp)
(require 'my-programming-lisp)
(require 'my-programming-nix)
(require 'my-programming-yaml)

(cl-eval-when (compile)
  (require 'apheleia)
  (require 'company)
  (require 'consult-flymake)
  (require 'display-line-numbers)
  (require 'eglot)
  (require 'eglot-booster)
  (require 'electric)
  (require 'flymake)
  (require 'hl-line)
  (require 'hl-todo)
  (require 'page-break-lines)
  (require 'parinfer-rust-mode)
  (require 'prog-mode)
  (require 'rainbow-delimiters)
  (require 'sideline)
  (require 'sideline-flymake)
  (require 'smartparens))

(defun my-inhibit-parinfer-rust-troublesome-modes (&rest _)
  "Disable all `parinfer-rust-troublesome-modes' to inhibit warnings."
  (dolist (mode parinfer-rust-troublesome-modes)
    (when (fboundp mode)
      (apply mode '(-1)))))



;;;
;; Appearance:

(setup display-line-numbers
  (:autoload display-line-numbers-mode))

(setup hl-line
  (:autoload hl-line-mode))

(setup hl-todo
  (:autoload hl-todo-mode))

(setup page-break-lines
  (:autoload page-break-lines-mode))

(setup rainbow-delimiters
  (:autoload rainbow-delimiters-mode))

(setup prog-mode
  (:hook
   #'display-line-numbers-mode ;; Show line numbers of buffer.
   #'hl-line-mode              ;; Highlight current line of buffer.
   #'hl-todo-mode              ;; Highlight TODO keywords.
   #'page-break-lines-mode     ;; Display ^L  as tidy horizontal lines.
   #'rainbow-delimiters-mode)) ;; Colorful brackets highlighting.



;;;
;; Complete:

(setup company
  (:autoload company-mode))

(setup prog-mode
  (:hook #'company-mode))



;;;
;; Diagnostics:

(setup consult-flymake
  (:autoload consult-flymake))

(setup sideline-flymake
  (:autoload sideline-flymake)
  (:when-loaded
    (:set sideline-flymake-display-mode 'point)))

(setup sideline
  (:autoload sideline-mode)
  (:when-loaded
    (:set (append sideline-backends-right) 'sideline-flymake)))

(setup flymake
  (:with-hook flymake-mode-hook
    (:hook
     #'sideline-mode
     #'(lambda ()
         (:with-map flymake-mode-map
           (:keymap-set
            "C-c !" #'consult-flymake))))))



;;;
;; Editing:

(setup electric
  (:when-loaded
    (push ?\^? electric-indent-chars)))

(setup smartparens
  (:autoload smartparens-mode)
  (:also-load smartparens-config))

(setup parinfer-rust-mode
  (:autoload parinfer-rust-mode parinfer-rust-mode-enable)
  (:advice-add
   parinfer-rust-mode-enable
   :before
   my-inhibit-parinfer-rust-troublesome-modes)
  (:when-loaded
    (:set parinfer-rust-preferred-mode "paren")))

(setup prog-mode
  (:hook
   #'electric-indent-local-mode ;; Auto reindentation.
   #'smartparens-mode))         ;; Auto insert paired paren.



;;;
;; Fold:

(setup hideshow
  (:autoload hs-minor-mode))

(setup prog-mode
  ;; Allow folding and unfolding of code blocks.
  (:hook #'hs-minor-mode))



;;;
;; Format:

(setup apheleia
  (:autoload apheleia-mode))

(setup prog-mode
  (:hook #'apheleia-mode))



;;;
;; LSP:

(setup eglot-booster
  (:autoload eglot-booster-mode)
  (:after eglot
    (eglot-booster-mode +1)))



(provide 'my-programming)
;;; my-programming.el ends here
