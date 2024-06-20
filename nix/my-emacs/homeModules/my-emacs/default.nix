{
  cell,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.programs.my-emacs;

  my-emacs = pkgs.writeScriptBin "my-emacs" ''
    #!${pkgs.runtimeShell}
    if [ -z "$1" ]; then
      exec ${cfg.package}/bin/emacsclient --create-frame --alternate-editor ${cfg.package}/bin/emacs
    else
      exec ${cfg.package}/bin/emacsclient --alternate-editor ${cfg.package}/bin/emacs "$@"
    fi
  '';
in
{
  options.programs.my-emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = mdDoc ''
        Whether use my-emacs.
      '';
    };

    defaultEditor = mkOption {
      type = types.bool;
      default = false;
      description = ''
        When enabled, configures emacsclient to be the default editor
        using the EDITOR environment variable.
      '';
    };

    extraBinaries = mkOption {
      type = with types; listOf package;
      default = with pkgs; [ ];
      description = ''
        Extra executable binaries will be add to my-emacs.
      '';
    };

    extraConfig = mkOption {
      type = types.str;
      default = "";
      description = mdDoc ''
        Extra configuration will be add to my-emacs.
      '';
    };

    extraEmacsPackages = mkOption {
      type = types.unspecified;
      default = epkgs: with epkgs; [ ];
      description = mdDoc ''
        Extra Emacs Lisp packages will be add to my-emacs.
      '';
    };

    extraFonts = mkOption {
      type = with types; listOf package;
      default = with pkgs; [ ];
      description = mdDoc ''
        Extra font packages will be add to my-emacs.
      '';
    };

    extraLibraries = mkOption {
      type = with types; listOf package;
      default = with pkgs; [ ];
      description = mdDoc ''
        Extra library packages will be add to my-emacs.
      '';
    };

    package = mkOption {
      type = types.package;
      default = cfg.scope.default;
      description = mdDoc ''
        my-emacs package will be used.
      '';
    };

    scope = mkOption {
      type = types.unspecified;
      default = pkgs.my-emacs;
      apply =
        s:
        s.override {
          inherit (cfg)
            extraBinaries
            extraConfig
            extraEmacsPackages
            extraFonts
            extraLibraries
            ;
        };
      description = mdDoc ''
        my-emacs package scope will be used.
      '';
    };

    userMail = mkOption {
      type = types.str;
      # TODO set a default value
      description = mdDoc ''
        Customize `user-mail-address` of GNU Emacs.
      '';
    };

    userName = mkOption {
      type = types.str;
      default = config.home.username;
      description = mdDoc ''
        Customize `user-full-name` of GNU Emacs.
      '';
    };

    variant = mkOption {
      type = types.enum [
        "nogui"
        "pgtk"
        "x11"
      ];
      default = "pgtk";
      description = mdDoc ''
        Which variant of my-emacs to use.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.variant == "pgtk") {
      programs = {
        my-emacs = {
          package = cfg.scope.default;
        };
      };
    })
    (mkIf (cfg.variant == "x11") {
      programs = {
        my-emacs = {
          package = cfg.scope.x11;
        };
      };
    })
    (mkIf (cfg.variant == "nogui") {
      programs = {
        my-emacs = {
          package = cfg.scope.nogui;
        };
      };
    })
    {
      home = {
        packages = [
          cfg.package
          my-emacs
        ] ++ cfg.scope.fonts;

        sessionVariables = {
          EDITOR = mkIf cfg.defaultEditor (mkOverride 900 "my-emacs");
        };
      };

      programs = {
        my-emacs = {
          extraConfig = ''
            (setq user-full-name "${cfg.userName}"
                  user-mail-address "${cfg.userMail}")
          '';
        };
      };
    }
  ]);
}
