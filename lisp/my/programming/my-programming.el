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
(require 'my-programming-javascript)
(require 'my-programming-lisp)
(require 'my-programming-typescript)
(require 'my-programming-nix)
(require 'my-programming-web)
(require 'my-programming-yaml)

(cl-eval-when (compile)
  (require 'apheleia)
  (require 'company)
  (require 'consult-flymake)
  (require 'display-line-numbers)
  (require 'eglot)
  (require 'eglot-booster)
  (require 'eldoc)
  (require 'electric)
  (require 'flymake)
  (require 'hl-line)
  (require 'hl-todo)
  (require 'parinfer-rust-mode)
  (require 'prog-mode)
  (require 'rainbow-delimiters)
  (require 'sideline)
  (require 'sideline-eldoc)
  (require 'sideline-flymake)
  (require 'sideline-lsp)
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

(setup rainbow-delimiters
  (:autoload rainbow-delimiters-mode))

(setup sideline
  (:autoload sideline-mode))

(setup prog-mode
  (:hook
   #'display-line-numbers-mode ;; Show line numbers of buffer.
   #'hl-line-mode              ;; Highlight current line of buffer.
   #'hl-todo-mode              ;; Highlight TODO keywords.
   #'rainbow-delimiters-mode   ;; Colorful brackets highlighting.
   #'sideline-mode))           ;; Sideline at current line.



;;;
;; Complete:

(setup company
  (:autoload company-mode))

(setup prog-mode
  (:hook #'company-mode))



;;;
;; Documentation:

(setup sideline-eldoc
  (:autoload sideline-eldoc))

(setup eldoc
  (:with-mode eldoc-mode
    (:hook
     #'(lambda ()
         (:local-set
          (append sideline-backends-left) 'sideline-eldoc)))))



;;;
;; Diagnostics:

(setup consult-flymake
  (:autoload consult-flymake))

(setup sideline-flymake
  (:autoload sideline-flymake)
  (:when-loaded
    (:set sideline-flymake-display-mode 'point)))

(setup flymake
  (:with-hook flymake-mode-hook
    (:hook
     #'(lambda ()
         (:local-set
          (append sideline-backends-right) 'sideline-flymake))
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

(setup sideline-lsp
  (:autoload sideline-lsp))

(setup eglot-booster
  (:autoload eglot-booster-mode))

(setup eglot
  (:with-hook eglot-managed-mode-hook
    (:hook
     #'eglot-booster-mode
     #'(lambda ()
         (:local-set
          (append sideline-backends-left) 'sideline-lsp)))))



(provide 'my-programming)
;;; my-programming.el ends here
