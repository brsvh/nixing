;;; my-security.el --- Security of My Emacs -*- lexical-binding: t -*-

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

;; This file ensures the security of my Emacs' network connections and
;; the management of secrets.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'auth-source)
  (require 'auth-source-pass)
  (require 'epg)
  (require 'nsm))

(defvar my-authinfo-file (my-data-path "authinfo")
  "My authinfo file.")

(defvar my-authinfo-gpg-file (my-data-path "authinfo.gpg")
  "My authinfo.gpg file.")



;;;
;; Auth:

(setup auth-source
  (:when-loaded
    (:set
     (prepend auth-sources) my-authinfo-gpg-file)
    (:also-load auth-source-pass)
    (:after auth-source-pass
      (auth-source-pass-enable))))



;;;
;; GnuPG:

(setup epg
  (:when-loaded
    ;; REVIEW this is a fix for GnuPG 2.4.1-2.4.3.
    (:advice-add epg-wait-for-status :override #'ignore)))



;;;
;; Network Security:

(setup nsm
  (:when-loaded
    (:set
     network-security-level 'medium
     nsm-settings-file (my-state-path "network-security.data"))))

(provide 'my-security)
;;; my-security.el ends here
