;;; my-email.el --- E-Mail support of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
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

;; This file provides support for sending and receiving my mails.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'mu4e-marker-icons)
  (require 'mu4e-update))



;;;
;; mu:

(setup mu4e-marker-icons
  (:autoload mu4e-marker-icons-mode)
  (:after mu4e
    (mu4e-marker-icons-mode +1)))

(setup mu4e-update
  (:snoc
   popper-reference-buffers "\\*mu4e-update\\*"))



(provide 'my-email)
;;; my-email.el ends here
