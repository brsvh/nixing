# overlays.unfree

## Usage

Simply use this overlay `brsvh.overlays.unfree` when import `nixpkgs`, and then install packages this overlay provided.

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
                overlays = [ brsvh.overlays.unfree ];
              };

              environment.systemPackageswith = with pkgs; [ wemeet ];
            }
          )
        ];
      };
    };
}
```

## Packages

### `pkgs.wemeet`

Tencent meeting.
