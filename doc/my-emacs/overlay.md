# overlays.my-emacs

## Usage

Simply use this overlay `brsvh.overlays.my-emacs` when import `nixpkgs`, and then install packages this overlay provided.

``` nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/unstable";
    brsvh.url = "github:brsvh/nixing";
    brsvh.inputs.nixpkgs.follows = "nixpkgs";
  };

  # OPTIONAL use my cachix binary cache.
  nixConfig = {
    extra-substituters = [ "https://brsvh.cachix.org" ];
    extra-trusted-public-keys = [ "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE=" ];
  };

  outputs =
    { nixpkgs, ... }@inputs:
    {
      nixosConfiguraitons.YOUR-CONFIGURATION = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                config.allowUnfree = true;
                overlays = [ brsvh.overlays.my-emacs ];
              };

              environment.systemPackageswith = with pkgs; [
                my-emacs.default # pgtk variant
                my-emacs.x11     # x11 variant
                my-emacs.nogui   # nogui variant
              ];
            }
          )
        ];
      };
    };
}
```

## Scopes

### `pkgs.my-emacs`

A scope based on GNU Emacs master branch.

### `pkgs.my-emacs-master`

A scope based on GNU Emacs master branch.

### `pkgs.my-emacs-unstable`

A scope based on GNU Emacs unstable branch.

### `pkgs.my-emacs-stable`

A scope based on GNU Emacs release branch.

## Packages

### `pkgs.<scope>.default`

My portable GNU Emacs configuration built with pure GTK support, performing better on Wayland.

### `pkgs.<scope>.nogui`

My portable GNU Emacs configuration without graphical support, typically used in my devshell.

### `pkgs.<scope>.x11`

my portable GNU Emacs configurations running on X.org.

## Scaffolds

### `pkgs.<scope>.binaries`

A packages list of executables with all runtime dependencies.

### `pkgs.<scope>.fonts`

A pakcages list of all the fonts will used in GNU Emacs.

### `pkgs.<scope>.instruments`

A symlink derivation of all packages in `pkgs.<scope>.binaries`.

### `pkgs.<scope>.libraries`

A packages list of all runtime dependency libraries.
