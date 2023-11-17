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
        kernel = {
          package = mkOption {
            type = types.raw;
            default = pkgs.linuxPackages;
            description = ''
              What version of kernel do you prefer to use?
            '';
          };

          modules = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = ''
              The set of kernel modules will be loaded in stage 2.
            '';
          };

          params = mkOption {
            type = types.listOf (types.strMatching
              ''([^"[:space:]]|"[^"]*")+'' //
            {
              name = "kernelParam";
              description = "string, with spaces inside double quotes";
            });
            default = [ ];
            description = ''
              The Kernel parameters will be used.
            '';
          };
        };
      };
    };
  };

  config = mkMerge
    [
      {
        boot = {
          kernelModules = cfg.kernel.modules;
          kernelPackages = cfg.kernel.package;
          kernelParams = cfg.kernel.params;
        };
      }
    ];
}
