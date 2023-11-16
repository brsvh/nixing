{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.system;
in
{
  options = {
    workstation = {
      system = {
        console = {
          font = mkOption {
            type = types.str;
            default = "eurlatgr";
            description = ''
              The default font that console(tty) used.
            '';
          };

          keymap = mkOption {
            type = types.str;
            default = "us";
            description = ''
              The default keymap that console(tty) used.
            '';
          };
        };
      };
    };
  };

  config = {
    console = mkMerge
      [
        {
          earlySetup = true;
          font = cfg.console.font;
          keyMap = mkDefault cfg.console.keymap;
        }
        (
          mkIf config.services.xserver.enable
            {
              useXkbConfig = true;
            }
        )
      ];
  };
}
