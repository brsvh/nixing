# This file is inspired by <https://github.com/ehllie/ez-configs>, and
# is adapted from its original code.
#
# Copyright (C) 2023-2024 Elizabeth Paź
# Copyright (C) 2024 Burgess Chang
#
# The original code is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at <https://mozilla.org/MPL/2.0/>.
#
# What is the difference compared to `ez-configs‘?
# 
#  - The `homeConfigurations’ are no longer automatically imported into
#    `nixosConfigurations‘.
#  - The `homeConfigurations' are now always standalone by default.
{
  inputs,
  lib,
  config,
  ...
}:
let

  inherit (builtins)
    elemAt
    pathExists
    readDir
    readFileType
    ;

  inherit (lib)
    concatMapAttrs
    literalExpression
    mapAttrs
    mkDefault
    mkOption
    types
    ;

  inherit (lib.strings)
    hasSuffix
    removeSuffix
    ;

  cfg = config.myConfigs;

  systemsWith =
    {
      defaultHost,
      extraSpecialArgs,
      myHomeModules,
      hostModules,
      myModules,
      os,
      specialArgs,
      userModules,
      users,
    }:
    hosts:
    mapAttrs (
      name: configModule:
      let
        hmModule =
          if inputs ? home-manager then
            (
              if os == "linux" then
                inputs.home-manager.nixosModules.default
              else
                inputs.home-manager.darwinModules.default
            )
          else
            throw ''
              home-manager input not found, please add a home-manager input to your flake.
            '';

        systemBuilder =
          if os == "linux" then
            (
              if inputs ? nixpkgs then
                inputs.nixpkgs.lib.nixosSystem
              else
                throw ''
                  nixpkgs input not found, but host ${name} present in nixosConfigurations directory.
                  Please add a nixpkgs input to your flake.
                ''
            )
          else
            (
              if inputs ? darwin then
                inputs.darwin.lib.darwinSystem
              else if inputs ? nix-darwin then
                inputs.nix-darwin.lib.darwinSystem
              else
                throw ''
                  darwin or nix-darwin input not found, but host ${name} present in darwinConfigurations directory.
                  Please add a darwin or nix-darwin input to your flake.
                ''
            );
      in
      systemBuilder {
        specialArgs = specialArgs // {
          inherit
            myModules
            ;
        };

        modules = [
          configModule
          hmModule
          {
            home-manager = {
              extraSpecialArgs = extraSpecialArgs // {
                myModules = myHomeModules;
              };
            };

            networking = {
              hostName = mkDefault "${name}";
            };
          }
        ];
      }
    ) hostModules;

  userImports =
    {
      myModules,
      stdenv,
      user,
      userModules,
    }:
    [
      (userModules.${user} or { })
    ];

  userConfigs =
    {
      extraSpecialArgs,
      myModules,
      userModules,
    }:
    users:
    concatMapAttrs (
      user: configModule:
      let
        inherit (users.${user})
          pkgs
          ;

        importModules =
          stdenv:
          userImports {
            inherit
              myModules
              stdenv
              user
              userModules
              ;
          };

        homeManagerConfiguration =
          if inputs ? home-manager then
            inputs.home-manager.lib.homeManagerConfiguration
          else
            throw ''
              home-manager input not found, but user ${user} present in homeConfigurations directory.
              Please add a home-manager input to your flake.
            '';
      in
      {
        ${user} = homeManagerConfiguration {
          inherit pkgs;

          extraSpecialArgs =
            extraSpecialArgs
            // {
              inherit myModules;
            }
            //
              # We still want to pass in osConfig even when there is none,
              # so that modules evaluate properly when using that argument
              {
                osConfig = { };
              };

          modules = importModules pkgs.stdenv;
        };
      }
    ) userModules;

  # This is a workaround the types.attrsOf (type.submodule ...) functionality.
  # We can't ensure that each host/ user present in the appropriate directory
  # is also present in the attrset, so we need to create a default module for it.
  # That way we can fallback to it if it's not present in the attrset.
  # Is there a better way to do this? Maybe defining a custom type?
  defaultSubmodule =
    submodule:
    concatMapAttrs (
      opt: optDef: if optDef ? default then { ${opt} = optDef.default; } else { }
    ) submodule.options;

  # Getting the first submodule seems to work, but not sure if it's the best way.
  defaultSubmoduleAttr = attrsType: defaultSubmodule (elemAt attrsType.getSubModules 0);

  hostOptions = system: {
    options = {

    };
  };

  userOptions = {
    options = {
      pkgs = mkOption {
        description = ''
          The package set with which to construct the homeManagerConfiguration.

          Non standalone user configurations will use the package set of the host system.
        '';

        example = literalExpression "import nixpkgs {system = \"x86_64-linux\"}";
        type = types.pkgs;
      };
    };
  };

  configurationOptions =
    configType:
    {
      modulesDirectory = mkOption {
        default = if cfg.root != null then "${cfg.root}/${configType}Modules" else ./unset-directory;
        defaultText = literalExpression "\"\${myConfigs.root}/${configType}Modules\"";

        description = ''
          The directory containing ${configType}Modules.
        '';

        type = types.path;
      };

      configurationsDirectory = mkOption {
        default = if cfg.root != null then "${cfg.root}/${configType}Configurations" else ./unset-directory;
        defaultText = literalExpression "\"\${myConfigs.root}/${configType}Configurations\"";

        description = ''
          The directory containing ${configType}Configurations.
        '';

        type = types.path;
      };

    }
    // (
      if configType == "home" then
        {
          extraSpecialArgs = mkOption {
            default = cfg.globalArgs;
            defaultText = literalExpression "myConfigs.globalArgs";

            description = ''
              Extra arguments to pass to all homeConfigurations.
            '';

            type = types.attrsOf types.anything;
          };

          users = mkOption {
            default = { };

            example = literalExpression ''
              {
                alice = {
                  pkgs = import nixpkgs { system = "x86_64-linux"; };
                };
              }
            '';

            description = ''
              Settings for creating homeConfigurations.

              It's not neccessary to specify this option to create flake outputs.
              It's only needed if you want to change the defaults for specific homeConfigurations.
            '';

            type = types.attrsOf (types.submodule userOptions);
          };
        }
      else
        {
          specialArgs = mkOption {
            default = cfg.globalArgs;
            defaultText = literalExpression "myConfigs.globalArgs";
            type = types.attrsOf types.anything;
            description = ''
              Extra arguments to pass to all ${configType}Configurations.
            '';
          };

          hosts = mkOption {
            default = { };
            type = types.attrsOf (types.submodule (hostOptions configType));
            example = literalExpression ''
              {
                hostA = {
                  arch = "aarch64
                };
              }
            '';
            description = ''
              Settings for creating ${configType}Configurations.

              It's not neccessary to specify this option to create flake outputs.
              It's only needed if you want to change the defaults for specific ${configType}Configurations.
            '';
          };
        }
    );

  readModules =
    dir:
    if pathExists "${dir}.nix" && readFileType "${dir}.nix" == "regular" then
      { default = dir; }
    else if pathExists dir && readFileType dir == "directory" then
      concatMapAttrs (
        entry: type:
        let
          dirDefault = "${dir}/${entry}/default.nix";
        in
        if type == "regular" && hasSuffix ".nix" entry then
          { ${removeSuffix ".nix" entry} = "${dir}/${entry}"; }
        else if pathExists dirDefault && readFileType dirDefault == "regular" then
          { ${entry} = dirDefault; }
        else
          { }
      ) (readDir dir)
    else
      { };
in
{
  options = {
    myConfigs = {
      root = mkOption {
        default = null;

        description = ''
          The root from which configurations and modules should be searched.
        '';

        example = literalExpression "./.";
        type = types.nullOr types.path;
      };

      globalArgs = mkOption {
        default = { };

        description = ''
          Extra arguments to pass to all configurations.
        '';

        example = literalExpression "{ inherit inputs; }";
        type = types.attrsOf types.anything;
      };

      home = configurationOptions "home";

      nixos = configurationOptions "nixos";

      darwin = configurationOptions "darwin";
    };
  };

  config = {
    flake = rec {
      darwinConfigurations = systemsWith {
        inherit (cfg.darwin)
          specialArgs
          ;

        inherit (cfg.home)
          extraSpecialArgs
          users
          ;

        os = "darwin";
        hostModules = readModules cfg.darwin.configurationsDirectory;
        defaultHost = defaultSubmoduleAttr ((configurationOptions "darwin").hosts.type);
        myModules = darwinModules;
        userModules = readModules cfg.home.configurationsDirectory;
        myHomeModules = homeModules;

      } cfg.darwin.hosts;

      darwinModules = readModules cfg.darwin.modulesDirectory;

      homeConfigurations = userConfigs {
        userModules = readModules cfg.home.configurationsDirectory;
        myModules = homeModules;
        inherit (cfg.home)
          extraSpecialArgs
          ;
      } cfg.home.users;

      homeModules = readModules cfg.home.modulesDirectory;

      nixosConfigurations = systemsWith {
        inherit (cfg.nixos)
          specialArgs
          ;

        inherit (cfg.home)
          extraSpecialArgs
          users
          ;

        os = "linux";
        hostModules = readModules cfg.nixos.configurationsDirectory;
        defaultHost = defaultSubmoduleAttr ((configurationOptions "nixos").hosts.type);
        myModules = nixosModules;
        userModules = readModules cfg.home.configurationsDirectory;
        myHomeModules = homeModules;
      } cfg.nixos.hosts;

      nixosModules = readModules cfg.nixos.modulesDirectory;
    };
  };
}
