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

  config = {
    boot = {
      initrd = {
        availableKernelModules = cfg.initrd.modules.implication;
        kernelModules = cfg.initrd.modules.explicitness;
      };
    };
  };
}
