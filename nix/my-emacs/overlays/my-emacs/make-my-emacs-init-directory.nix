{
  lib,
  linkFarm,
  stdenv,
  pkgs,
  projectRoot,
  writeText,
}:
emacs: config:
with builtins;
with lib;
let
  my-interlude = writeText "my-interlude.el" ''
    ;;; my-interlude.el --- Interlude of My Emacs -*- lexical-binding: t; -*-

    ;; Copyright (C) 2022-2024 Burgess Chang

    ;; Author: Burgess Chang <bsc@brsvh.org>
    ;; Keywords: local
    ;; Package-Requires: ((emacs "29.1"))
    ;; URL: https://github.com/brsvh/my-emacs
    ;; Version: 0.1.0

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
       parinfer-rust-library "${pkgs.parinfer-rust-emacs}/lib/libparinfer_rust.so"
       parinfer-rust-library-directory "${pkgs.parinfer-rust-emacs}/lib/"))

    ${config}

    (provide 'my-interlude)
    ;;; my-interlude.el ends here
  '';
in
stdenv.mkDerivation {
  name = "my-emacs-init-directory";

  buildInputs = [ emacs ];

  src = projectRoot;

  phases = [
    "unpackPhase"
    "buildPhase"
    "installPhase"
  ];

  unpackPhase = ''
    :
  '';

  buildPhase = ''
    mkdir -p $TMPDIR/native-lisp;

    cp -r $src/{etc,lisp} $TMPDIR/;

    chmod -R u+w $TMPDIR;

    mv $TMPDIR/lisp/{early-,}init.el $TMPDIR/;
    cat ${my-interlude} > $TMPDIR/lisp/my/my-interlude.el

    mkdir -p $TMPDIR/.local;

    HOME=$TMPDIR

    find $TMPDIR -type f -name "*.el" -exec ${emacs}/bin/emacs --debug-init --no-init-file --no-site-file --eval "(progn (setq gc-cons-threshold most-positive-fixnum load-prefer-newer t) (push \"$TMPDIR/lisp\" load-path) (let ((default-directory \"$TMPDIR/lisp\")) (normal-top-level-add-subdirs-to-load-path)) (setq my-cache-directory \"$TMPDIR/.local\" my-config-directory \"$TMPDIR/.local\" my-data-directory \"$TMPDIR/.local\" my-state-directory \"$TMPDIR/.local\" my-prelude-inhibit-update-load-path t)) " --batch -f batch-byte-compile {} \;

    find $TMPDIR -type f -name "*.el" -exec ${emacs}/bin/emacs --debug-init --no-init-file --no-site-file --eval "(progn (setq gc-cons-threshold most-positive-fixnum load-prefer-newer t) (push \"$TMPDIR/lisp\" load-path) (let ((default-directory \"$TMPDIR/lisp\")) (normal-top-level-add-subdirs-to-load-path)) (push "\"$TMPDIR/native-lisp/\"" native-comp-eln-load-path) (setq my-cache-directory \"$TMPDIR/.local\" my-config-directory \"$TMPDIR/.local\" my-data-directory \"$TMPDIR/.local\" my-state-directory \"$TMPDIR/.local\" my-prelude-inhibit-update-load-path t)) " --batch -f batch-native-compile {} \;
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out;

    cp $TMPDIR/{early-,}init.el{,c} $out/;
    cp -r $TMPDIR/{etc,lisp,native-lisp} $out/;
    cp $src/.dir-locals.el $out/;

    runHook postInstall
  '';
}
