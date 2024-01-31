{ config
, lib
, withSystem
, ...
}:
with builtins;
with lib;
let
  cfg = config.configurations;

  homeOpts =
    { config
    , name
    , ...
    } @ opts:
    let
      cfg' = config;
    in
    {
      options = {
        home-manager = mkOption {
          type = types.unspecified;
          description = ''
            Which home-manager will be used for building the
            homeConfiguration.

            Required to be set per-profile or using
            `configurations.default.home.home-manager`.
          '';
          default = cfg.default.home.home-manager;
        };

        modules = mkOption {
          type = types.listOf types.unspecified;
          description = ''
            List of modules to include in the homeConfiguration.
          '';
          default = [ ];
        };

        nixpkgs = mkOption {
          type = types.unspecified;
          description = ''
            nixpkgs input to use for building the homeConfiguration.

            Required to be set per-profile or using
            `configurations.default.home.nixpkgs`.
          '';
          default = cfg.default.home.nixpkgs;
        };

        specialArgs = mkOption {
          type = types.attrsOf types.unspecified;
          description = ''
            `extraSpecialArgs` passed to the homeConfiguration.
          '';
          default = { };
        };

        system = mkOption {
          type = types.enum platforms.all;
          description = ''
            system used for building the homeConfiguration.
          '';
          default = cfg.default.home.system;
        };

        stateVersion = mkOption {
          type = types.str;
          description = ''
            home-manager state version.
          '';
          default = cfg.default.home.stateVersion;
        };

        username = mkOption {
          type = types.str;
          description = ''
            The username passed to home-manager, or `home.username`.

            Defaults to read from the first name of
            homeConfiguration, like:
               foo     -> foo
               foo@bar -> foo
          '';
          default = builtins.elemAt (strings.split "@" name) 0;
        };

        homeDirectory = mkOption {
          type = types.str;
          description = ''
            The home directory passed to home-manager, or set the
            `home.homeDirectory`.
          '';
          default =
            if
              (
                with cfg'.nixpkgs;
                legacyPackages.${cfg'.system}.stdenv.isDarwin
              )
            then "/Users/${cfg'.username}"
            else "/home/${cfg'.username}";
        };

        finalHomeConfiguration = mkOption {
          type = types.unspecified;
          description = ''
            The final homeConfiguration.
          '';
        };
      };

      config = {
        finalHomeConfiguration =
          withSystem
            cfg'.system
            (
              { system
              , ...
              } @ ctx:
              cfg'.home-manager.lib.homeManagerConfiguration {
                pkgs = cfg'.nixpkgs.legacyPackages."${system}";

                extraSpecialArgs =
                  recursiveUpdate
                    cfg.global.home.specialArgs
                    cfg'.specialArgs;

                modules =
                  cfg.global.home.modules ++
                  [
                    {
                      home = {
                        inherit (cfg')
                          homeDirectory
                          stateVersion
                          username;
                      };

                      programs = {
                        home-manager = {
                          enable = true;
                          path = mkForce "${cfg'.home-manager}";
                        };
                      };
                    }
                  ] ++
                  cfg'.modules;
              }
            );
      };
    };
in
{
  options = {
    configurations = {
      default = {
        home = {
          home-manager = mkOption {
            type = types.unspecified;
            description = ''
              The default home-manager input.
            '';
          };

          nixpkgs = mkOption {
            type = types.unspecified;
            description = ''
              The default nixpkgs input.
            '';
          };

          system = mkOption {
            type = types.enum platforms.all;
            description = ''
              The default system.
            '';
            default = "x86_64-linux";
          };

          stateVersion = mkOption {
            type = types.str;
            description = ''
              home-manager state version.
            '';
          };
        };
      };

      global = {
        home = {
          modules = mkOption {
            type = types.unspecified;
            description = ''
              List of modules to include in all homeConfigurations.
            '';
            default = [ ];
          };

          specialArgs = mkOption {
            type = types.attrsOf types.unspecified;
            description = ''
              `extraSpecialArgs` passed to all homeConfigurations.
            '';
            default = { };
          };
        };

      };

      home = mkOption {
        type = types.attrsOf (types.submodule homeOpts);
        description = ''
          The collection of all homeConfigurations.
        '';
      };
    };
  };
}
