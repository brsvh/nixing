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
        initrd = {
          quiet = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Limit the verbosity of stage 1 boot process.
            '';
          };

          modules = {
            implication = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = ''
                The set of kernel modules in the initial ramdisk used
                during the boot process.
              '';
            };

            explicitness = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = ''
                The set of kernel modules in the initial ramdisk used
                during the boot process.
              '';
            };
          };
        };
      };
    };
  };

  config = mkMerge
    [
      {
        boot = {
          initrd = {
            verbose = ! cfg.initrd.quiet;
            availableKernelModules = cfg.initrd.modules.implication;
            kernelModules = cfg.initrd.modules.explicitness;
          };
        };
      }
    ];
}
