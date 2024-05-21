;;; my-interlude.el --- Interlude of My Emacs  -*- lexical-binding: t -*-

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

;; This file is loaded during my Emacs initialization.

;; NOTE:
;;
;; This file is fake, it will be override in Nix derivation making.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'parinfer-rust-mode))

(setup treesit-grammars
  (:snoc
   treesit-extra-load-path
   "${
         linkFarm "treesit-grammars" (
           map
             (drv: {
               name = "lib${removeSuffix "-grammar" (getName drv)}${stdenv.targetPlatform.extensions.sharedLibrary}";
               path = "${drv}/parser";
             })
             (
               pipe pkgs.tree-sitter-grammars [
                 (filterAttrs (name: _: name != "recurseForDerivations"))
                 attrValues
               ]
             )
         )
       }"))

(setup parinfer-rust
  (:set
   parinfer-rust-auto-download nil
   parinfer-rust-library "${pkgs.parinfer-rust}/lib/libparinfer_rust.so"
   parinfer-rust-library-directory "${pkgs.parinfer-rust}/lib/"))

${config}

(provide 'my-interlude)
;;; my-interlude.el ends here
