# my-emacs

In this repository, I provide my portable Emacs configuration package,
built using Nix. It uses Nix to manage all Emacs Lisp packages and
runtime dependencies.

## Overview

my-emacs by default includes a scope, and this scope provides the following:

- `binaries`, a list of executables with all runtime dependencies.
- `default`, built with pure GTK support, performing better on Wayland.
- `fonts`, a list of all the fonts I use.
- `instruments`, symlink derivations of all packages in `binaries`.
- `libraries`, all runtime dependency libraries.
- `nogui`, a variant without graphical support, typically used in my devshell.
- `x11`, the conventional variant running on X.org.

## Getting Started

If you wish to directly glimpse my Emacs configuration, you need to
ensure that you have installed the latest Nix, and then simply execute
the following commands.

If you are a Wayland user, you can use the pgtk variant.

``` shell
nix run github:brsvh/nixing#my-emacs-pgtk
```

If you are a character-only (tty) user, you can use the nogui variant.

``` shell
nix run github:brsvh/nixing#my-emacs-nogui
```

If you need to run on X11, you can use the x11 variant.

``` shell
nix run github:brsvh/nixing#my-emacs-x11
```

And check the [Cheat Sheet] to see which shortcuts I have used.

[Cheat Sheet]: ./cheat-sheet.md

### Direct installation

Add the `emacs` overlay to your `nixosConfigurations` or
`homeConfigurations` to install my Emacs configuration.

``` nix
{
  nixConfig = {
    extra-substituters = [ "https://brsvh.cachix.org" ];

    extra-trusted-public-keys = [ "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE=" ];
  };

  inputs = {
    home-manager.url = "github:nix-community/home-manager/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    brsvh.url = "github:brsvh/nixing/main";
  };

  output =
    {
      brsvh,
      home-manager,
      nixpkgs,
      ...
    }@inputs:
    {
      homeConfigurations.YOUR-CONFIGURATION-NAME = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        modules = [
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                overlays = [ brsvh.overlays.emacs ];
              };

              home = {
                packages = with pkgs; [
                  my-emacs.default # pgtk variant
                  my-emacs.nogui   # nogui variant
                  my-emacs.x11     # x11 variant
                ];
              };
            }
          )
        ];
      };

      nixosConfigurations.YOUR-CONFIGURATOIN-NAME = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                overlays = [ nixing.overlays.emacs ];
              };

              environment = {
                systemPackages = with pkgs; [
                  my-emacs.default # pgtk variant
                  my-emacs.nogui   # nogui variant
                  my-emacs.x11     # x11 variant
                ];
              };
            }
          )
        ];
      };
    };
}
```

### Installation via nixosModules

Import the `brsvh.nixosModules.emacs` or `brsvh.homeModules` for quickly
configure available options.

``` nix
{
  nixConfig = {
    extra-substituters = [ "https://brsvh.cachix.org" ];

    extra-trusted-public-keys = [ "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE=" ];
  };

  inputs = {
    home-manager.url = "github:nix-community/home-manager/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    brsvh.url = "github:brsvh/nixing/main";
  };

  output =
    {
      brsvh,
      home-manager,
      nixpkgs,
      ...
    }@inputs:
    {
      homeConfigurations.YOUR-CONFIGURATION-NAME = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        modules = [
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                overlays = [ brsvh.overlays.emacs ];
              };

              imports = [ brsvh.homeModules.emacs ];

              programs = {
                my-emacs = {
                  enable = true;
                  variant = "pgtk"; # Use pgtk variant.
                };
              };
            }
          )
        ];
      };

      nixosConfigurations.YOUR-CONFIGURATOIN-NAME = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                overlays = [ nixing.overlays.emacs ];
              };

              imports = [ brsvh.nixosModules.emacs ];

              programs = {
                my-emacs = {
                  enable = true;
                  variant = "x11"; # Use x11 variant.
                };
              };
            }
          )
        ];
      };
    };
}
```
