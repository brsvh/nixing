;;; my-prelude.el --- Prelude of My Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1") (my-lib "0.1.0"))
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

;; The prelude of my Emacs configuration, setup matters of utmost
;; importance before initialization.

;; This file should be loaded at the `early-init-file' or the very
;; beginning of either the `user-init-file'.

;;; Code:

(defconst my-etc-directory
  (expand-file-name "etc/" user-emacs-directory)
  "Directory beneath which my configuration files are placed.")

(defvar my-initial-file-name-handler-alist file-name-handler-alist
  "The initial value of `file-name-handler-alist'.")

(defconst my-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Directory beneath which my Emacs Lisp files are placed.")

(defconst my-native-lisp-directory
  (expand-file-name "native-lisp/" user-emacs-directory)
  "Directory beneath which third-party Emacs Lisp files are placed.")

(defconst my-prelude-eln-load-path
  (let ((base (if (not (memq system-type '(ms-dos windows-nt cygwin)))
                  (or (getenv "XDG_CACHE_HOME") "~/.cache/")
                user-emacs-directory)))
    (list
     (expand-file-name "my-emacs/eln-cache/" base)
     my-native-lisp-directory))
  "Directory to look for natively-compiled *.eln files.")

(defvar my-prelude-frame-alist '((height . 40)
                                 (horizontal-scroll-bars . nil)
                                 (internal-border-width . 6)
                                 (menu-bar-lines . nil)
                                 (right-divider-width . 6)
                                 (tool-bar-lines . nil)
                                 (vertical-scroll-bars . nil)
                                 (width . 120))
  "Frame parameters for setup frame creation.")

(defvar my-prelude-inhibit-update-load-path nil
  "Inhibit update `load-path` if non-nil.")

(defvar my-prelude-load-path
  (list my-lisp-directory)
  "List of directories to search for self-maintained Emacs Lisp files.")

(defun my-prelude-add-subdirs-to-load-path (dir)
  "Add subdirectories of DIR to `load-path'."
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defmacro my-prelude-autoload (func feature)
  "Autoload FUNC from FEATURE."
  `(unless (fboundp ',func)
     (autoload #',func ,(symbol-name feature) nil t)))

(defun my-prelude-update-load-path (&rest _)
  "Update necessary paths to `load-path'."
  (dolist (dir (nreverse my-prelude-load-path))
    (push dir load-path)
    (my-prelude-add-subdirs-to-load-path dir)))

(defun my-reset-gc-threshold (&rest _)
  "Reset `gc-cons-threshold'."
  (set-default-toplevel-value
   'gc-cons-threshold
   (default-toplevel-value 'gc-cons-threshold)))

(defun my-reset-mode-line (&rest _)
  "Reset Mode Line."
  (set-default-toplevel-value
   'mode-line-format
   (default-toplevel-value 'mode-line-format)))

(defun my-restore-file-name-handler (&rest _)
  "Restore the appropriate value of `file-name-handler-alist'."
  (let ((prev my-initial-file-name-handler-alist)
        (current file-name-handler-alist))
    (set-default-toplevel-value
     'file-name-handler-alist
     (delete-dups (append current prev)))))



;;;
;; Path:

;; Add customized paths for user Emacs Lisp Native Compilation file
;; storage.
(dolist (path (reverse my-prelude-eln-load-path))
  (setq native-comp-eln-load-path
        (cons path native-comp-eln-load-path)))

;; Let my Emacs Lisp files can be find.
(unless my-prelude-inhibit-update-load-path
  (my-prelude-update-load-path))



;;;
;; Runtime:

(require 'cl-lib)
(require 'my-lib)

;; Inhibit save customizations to `user-init-file`.
(setq custom-file my-custom-file)

;; Redirect the location of directory containing the user’s Emacs Lisp
;; packages.
(setq package-user-dir (my-data-path (format "elpa/%s/" emacs-version))
      package-quickstart-file (my-path package-user-dir "autoloads.el"))

;; Reset frame layout.
(setq default-frame-alist
      (delete-dups (append default-frame-alist
                           my-prelude-frame-alist)))

;;;
;; Startup optimizations:

(cl-eval-when (compile)
  (require 'gcmh))

(progn                                            ;; Garbage collections
  (my-prelude-autoload gcmh-mode gcmh)

  ;; Activate intelligent garbage collections.
  (add-hook 'after-init-hook #'gcmh-mode 100)

  ;; Restore the garbage collection threshold to its default value.
  (advice-add 'gcmh-mode :before #'my-reset-gc-threshold))

(unless (daemonp)                                   ;; File name handler
  ;; Keep the initial file name handler.
  (put 'file-name-handler-alist
       'initial-value
       my-initial-file-name-handler-alist)

  ;; Reduce the number of suffixes supported by file name handler during
  ;; the startup to save overhead.
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (eval-when-compile
         (locate-file-internal "calc-loaddefs.el" load-path))
       nil
     (list (rassq 'jka-compr-handler
                  my-initial-file-name-handler-alist))))

  ;; Restore the appropriate value of `file-name-handler-alist`.
  (add-hook 'after-init-hook #'my-restore-file-name-handler 100))

(unless noninteractive                                     ;; Appearance
  ;; Avoid window resizing caused by font changes at the startup.
  (setq frame-inhibit-implied-resize t)

  ;; Keep the initial Mode Line.
  (put 'mode-line-format
       'initial-value
       (default-toplevel-value 'mode-line-format))

  ;; Avoid Mode Line the rendering during the configuration evaluation
  ;; process.
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf (setq mode-line-format nil))))

;;;
;; Benchmark:

(cl-eval-when (compile)
  (require 'benchmark-init))

;; Activate `benchmark-init' as early as possible to capture loading
;; information during the startup process.
(my-prelude-autoload benchmark-init/activate benchmark-init)
(benchmark-init/activate)

;; Deactivate `benchmark-init' at the very end of `after-init-hook'.
(my-prelude-autoload benchmark-init/deactivate benchmark-init)
(add-hook 'after-init-hook #'benchmark-init/deactivate 100)



(provide 'my-prelude)
;;; my-prelude.el ends here
