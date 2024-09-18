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
  (require 'auth-source)
  (require 'message)
  (require 'rmail)
  (require 'rmailmm)
  (require 'sendmail)
  (require 'smtpmail)
  (require 'url-util))



(defvar my-email-account user-mail-address
  "The default user name of E-Mail account.")

(defvar my-email-dir (my-data-path "mail/")
  "The default maildir of my E-Mails.")

(defvar my-email-recive-server-host ""
  "The default E-Mail receiving server host of `my-email-user'.")

(defvar my-email-recive-server-port 993
  "The default E-Mail receiving server port of `my-email-user'.")

(defvar my-email-recive-protocol "imaps"
  "The default protocol for receiving E-Mails.")

(defvar my-email-recive-url ""
  "The default mail url which specifying a remote mail account.")

(defvar my-email-send-server-host ""
  "The default E-Mail sending server of `my-email-user'.")

(defvar my-email-send-server-port 465
  "The default E-Mail sending server port of `my-email-user'.")

(defvar my-email-send-protocol "smtp"
  "The default protocol for sending E-Mails.")

(defun my-email-account-setup (&rest _)
  "Setup my default E-Mail account."
  (let* ((address my-email-account)
         (rproto my-email-recive-protocol)
         (rport my-email-recive-server-port)
         (rinfo (car (auth-source-search :max 1
                                         :port rport
                                         :require '(:user :secret :host)
                                         :user address)))
         (sport my-email-send-server-port)
         (sinfo (car (auth-source-search :max 1
                                         :port sport
                                         :require '(:user :secret :host)
                                         :user address)))
         (user (url-hexify-string address))
         (rhost (plist-get rinfo :host))
         (shost (plist-get sinfo :host)))
    (when rhost
      (setq my-email-recive-server-host rhost
            my-email-recive-url (format "%s://%s@%s:%s"
                                        rproto user rhost rport)))
    (when shost
      (setq my-email-send-server-host shost))))



;;;
;; Message:

(setup message
  (:when-loaded
    (:set
     message-default-headers (concat "Fcc: "
                                     (my-path* my-email-dir "sent")))))



;;;
;; Receiving:

(setup rmail
  (:when-loaded
    (my-email-account-setup)
    (:set
     rmail-file-name (my-path* my-email-dir "inbox")
     rmail-mime-prefer-html nil
     (prepend rmail-primary-inbox-list) my-email-recive-url
     rmail-remote-password-required t
     rmail-secondary-file-directory my-email-dir)))



;;;
;; Sending:

(setup smtpmail
  (:when-loaded
    (my-email-account-setup)
    (:set
     smtpmail-local-domain "localdmain"
     smtpmail-smtp-server my-email-send-server-host
     smtpmail-smtp-service my-email-send-server-port
     smtpmail-smtp-user my-email-account
     smtpmail-stream-type 'ssl)))

(setup sendmail
  (:when-loaded
    (:set send-mail-function 'smtpmail-send-it)))



(provide 'my-email)
;;; my-email.el ends here
