{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mdDoc
    mkIf
    mkMerge
    mkOption
    types
    ;

  dirLocals = ''
    ;;; Directory Local Variables            -*- no-byte-compile: t -*-
    ;;; For more information see (info "(emacs) Directory Variables")

    ${config.programs.emacs.dirLocals}
  '';

  earlyInit = ''
    ;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

    ${config.programs.emacs.extraEarlyInitHeader}

    ;; This file is not part of GNU Emacs.

    ;; This file is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published
    ;; by the Free Software Foundation, either version 3 of the License,
    ;; or (at your option) any later version.

    ;; This file is distributed in the hope that it will be useful, but
    ;; WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    ;; General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

    ;;; Commentary:

    ;; This file is load before normal init file is loaded.

    ;;; Code:

    ${config.programs.emacs.extraEarlyInitConfig}

    (provide 'early-init)
    ;;; early-init.el ends here
  '';

  init = ''
    ;;; init.el --- Init File -*- lexical-binding: t; -*-

    ${config.programs.emacs.extraInitHeader}

    ;; This file is not part of GNU Emacs.

    ;; This file is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published
    ;; by the Free Software Foundation, either version 3 of the License,
    ;; or (at your option) any later version.

    ;; This file is distributed in the hope that it will be useful, but
    ;; WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    ;; General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

    ;;; Commentary:

    ;; This file is the first loaded file after Emacs is started.

    ;;; Code:

    ${config.programs.emacs.extraInitConfig}

    (provide 'init)
    ;;; init.el ends here
  '';

  magicEmacs =
    let
      inherit (pkgs)
        runtimeShell
        writeScriptBin
        ;

      emacs = config.programs.emacs.finalPackage;
    in
    writeScriptBin "emacs" ''
      #!${runtimeShell}
      if [ -z "$1" ]; then
        exec ${emacs}/bin/emacsclient --create-frame --alternate-editor ${emacs}/bin/emacs
      else
        exec ${emacs}/bin/emacsclient --alternate-editor ${emacs}/bin/emacs "$@"
      fi
    '';
in
{

  options = {
    programs = {
      emacs = {
        dirLocals = mkOption {
          default = "";

          description = mdDoc ''
            The .dir-locals.el will be add to .config/emacs.
          '';

          type = types.lines;
        };

        extraDependencies = mkOption {
          default = [ ];

          description = mdDoc ''
            The extra packages will be add to PATH.
          '';

          type = with types; listOf package;
        };

        extraEarlyInitConfig = mkOption {
          default = "";

          description = mdDoc ''
            The extra configuration will be add to  `early-init-file`.
          '';

          type = types.lines;
        };

        extraEarlyInitHeader = mkOption {
          default = "";

          description = mdDoc ''
            The library header  will be add to  `early-init-file`.
          '';

          type = types.lines;
        };

        extraInitConfig = mkOption {
          default = "";

          description = mdDoc ''
            The extra configuration will be add to  `user-init-file`.
          '';

          type = types.lines;
        };

        extraInitHeader = mkOption {
          default = "";

          description = mdDoc ''
            The library header will be add to  `user-init-file`.
          '';

          type = types.lines;
        };

        isDefaultEditor = mkOption {
          default = false;

          description = ''
            Set Emacs as default editor.
          '';

          type = types.bool;
        };
      };
    };
  };

  config = mkMerge [
    (mkIf config.programs.emacs.enable {
      home = {
        packages = config.programs.emacs.extraDependencies;
      };

      xdg = {
        configFile = {
          "emacs/.dir-locals.el" = {
            text = dirLocals;
          };

          "emacs/early-init.el" = {
            text = earlyInit;
          };

          "emacs/init.el" = {
            text = init;
          };
        };
      };
    })
    (mkIf (config.programs.emacs.enable && config.programs.emacs.isDefaultEditor) {
      home = {
        sessionVariables = {
          EDITOR = "${magicEmacs}/bin/emacs";
        };
      };
    })
  ];
}
