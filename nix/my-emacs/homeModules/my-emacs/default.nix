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

    variant = mkOption {
      type = types.enum [
        "nogui"
        "pgtk"
        "x11"
      ];
      default = "pgkt";
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
        packages = [ cfg.package ] ++ cfg.scope.fonts;
      };
    }
  ]);
}
