{ config
, lib
, withSystem
, ...
}:
with builtins;
with lib;
let
  cfg = config.configurations;
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

      home = mkOption {
        type = types.attrsOf (types.submodule (
          { name
          , config
          , ...
          }:
          {
            options = {
              home-manager = mkOption {
                type = types.unspecified;
                description = ''
                  home-manager input to use for building the
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
                      with config.nixpkgs;
                      legacyPackages.${config.system}.stdenv.isDarwin
                    )
                  then "/Users/${config.username}"
                  else "/home/${config.username}";
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
                withSystem config.system
                  (
                    ctx @ { system, ... }:
                    config.home-manager.lib.homeManagerConfiguration {
                      pkgs = config.nixpkgs.legacyPackages."${system}";
                      extraSpecialArgs =
                        recursiveUpdate
                          cfg.global.home.specialArgs
                          config.specialArgs;
                      modules =
                        cfg.global.home.modules
                        ++ [
                          {
                            home = {
                              inherit (config)
                                homeDirectory
                                stateVersion
                                username;
                            };
                          }
                        ]
                        ++ config.modules;
                    }
                  );
            };
          }
        ));
        description = ''
          The collection of all homeConfigurations.
        '';
      };

      nixos = mkOption {
        type = types.attrsOf (types.submodule (
          { config
          , name
          , ...
          }:
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
                  List of modules to include in the nixosConfiguration
                '';
                default = [ ];
              };

              nixpkgs = mkOption {
                type = types.unspecified;
                description = ''
                  nixpkgs input to use for building the nixosConfiguration
                '';
                default = cfg.default.nixos.nixpkgs;
              };

              specialArgs = mkOption {
                type = types.attrsOf types.unspecified;
                description = ''
                  `specialArgs` passed to the nixosConfiguration
                '';
                default = { };
              };

              system = mkOption {
                type = types.enum platforms.linux;
                description = ''
                  The system used when defining the nixosConfiguration
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

              finalNixOSConfiguration = mkOption {
                type = types.unspecified;
                description = ''
                  The final nixosConfiguration.
                '';
              };
            };

            config = {
              finalNixOSConfiguration =
                withSystem config.system
                  (
                    ctx @ { system, ... }:
                    config.nixpkgs.lib.nixosSystem {
                      inherit system;
                      modules =
                        cfg.global.nixos.modules
                        ++ [
                          {
                            nixpkgs = {
                              hostPlatform = system;
                            };
                            networking = {
                              inherit (config) domain hostName;
                            };
                            system = {
                              inherit (config) stateVersion;
                            };
                          }
                        ]
                        ++ config.modules;
                      specialArgs =
                        recursiveUpdate
                          cfg.global.nixos.specialArgs
                          config.specialArgs;
                    }
                  );
            };
          }
        ));
        description = ''
          The collection of all nixosConfigurations.
        '';
      };
    };
  };
}
