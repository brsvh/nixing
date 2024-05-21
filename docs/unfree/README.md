# unfree

This repository provides some non-free software that can be installed via the unfree overlay.

## Getting Started

You can run them directly from this repository's flake. For example, to run `wemeet`:

```nix
nix run github:brsvh/nixing#wemeet
```

Add the unfree overlay to your nixosConfigurations or homeConfigurations to install them.

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
                config = {
                  allowUnfree = true;
                };

                overlays = [ brsvh.overlays.unfree ];
              };

              home = {
                packages = with pkgs; [ wemeet ];
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
                config = {
                  allowUnfree = true;
                };

                overlays = [ nixing.overlays.unfree ];
              };

              environment = {
                systemPackages = with pkgs; [ wemeet ];
              };
            }
          )
        ];
      };
    };
}
```

`
## Pakcage list

| Package  | Description     |
|----------|-----------------|
| `wemeet` | Tencent Meeting |
