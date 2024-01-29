{ config
, lib
, withSystem
, ...
}:
with builtins;
with lib;
let
  cfg = config.configurations;

  nixosOpts =
    { config
    , name
    , ...
    } @ opts:
    let
      cfg' = config;
    in
    {
      options = {
        domain = mkOption {
          default = "localdomain";
          example = "example.org";
          type = types.nullOr types.str;
          description = ''
            Set `networking.domain` of the nixosConfiguration.

            Default to configuration name with .localdomain suffix.
          '';
        };

        hostName = mkOption {
          type = types.str;
          description = ''
            Set `networking.hostName` of the nixosConfiguration.

            Defaults to configuration name.
          '';
          default = name;
        };

        modules = mkOption {
          type = types.listOf types.unspecified;
          description = ''
            List of modules to include in the nixosConfiguration.
          '';
          default = [ ];
        };

        nixpkgs = mkOption {
          type = types.unspecified;
          description = ''
            nixpkgs input to use for building the nixosConfiguration.
          '';
          default = cfg.default.nixos.nixpkgs;
        };

        specialArgs = mkOption {
          type = types.attrsOf types.unspecified;
          description = ''
            `specialArgs` passed to the nixosConfiguration.
          '';
          default = { };
        };

        system = mkOption {
          type = types.enum platforms.linux;
          description = ''
            The system used when defining the nixosConfiguration.
          '';
          default = cfg.default.nixos.system;
        };

        stateVersion = mkOption {
          type = types.str;
          description = ''
            NixOS state version.
          '';
          default = cfg.default.nixos.stateVersion;
        };

        zone = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "Asia/Shanghai";
          description = ''
            The time region.
          '';
        };

        finalNixOSConfiguration = mkOption {
          type = types.unspecified;
          description = ''
            The final nixosConfiguration.
          '';
        };
      };

      config = {
        finalNixOSConfiguration =
          withSystem
            cfg'.system
            (
              { system
              , ...
              } @ ctx:
              cfg'.nixpkgs.lib.nixosSystem {
                inherit system;

                modules =
                  cfg.global.nixos.modules ++
                  [
                    {
                      nixpkgs = {
                        hostPlatform = system;
                      };
                      networking = {
                        inherit (cfg') domain hostName;
                      };
                      system = {
                        inherit (cfg') stateVersion;
                      };
                      time = {
                        timeZone = cfg'.zone;
                      };
                    }
                  ] ++
                  cfg'.modules;

                specialArgs =
                  recursiveUpdate
                    cfg.global.nixos.specialArgs
                    cfg'.specialArgs;
              }
            );
      };
    };
in
{
  options = {
    configurations = {
      default = {
        nixos = {
          nixpkgs = mkOption {
            type = types.unspecified;
            description = ''
              The default nixpkgs input.
            '';
          };

          system = mkOption {
            type = types.enum platforms.linux;
            description = ''
              The default system.
            '';
            default = "x86_64-linux";
          };

          stateVersion = mkOption {
            type = types.str;
            description = ''
              The default NixOS state version.
            '';
          };
        };
      };

      global = {
        nixos = {
          modules = mkOption {
            type = types.unspecified;
            description = ''
              List of modules to include in all nixosConfigurations.
            '';
            default = [ ];
          };

          specialArgs = mkOption {
            type = types.attrsOf types.unspecified;
            description = ''
              `specialArgs` passed to all nixosConfigurations.
            '';
            default = { };
          };
        };
      };

      nixos = mkOption {
        type = types.attrsOf (types.submodule nixosOpts);
        description = ''
          The collection of all nixosConfigurations.
        '';
      };
    };
  };
}  
