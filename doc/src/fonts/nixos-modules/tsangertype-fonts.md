# nixosModules.tsangertype-fonts

## Usage

```nix
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
          brsvh.nixosModules.tsangertype-fonts
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                config.allowUnfree = true;
                overlays = [ brsvh.overlays.proprius-fonts ];
              };
              fonts = {
                tsangertype-fonts = {
                  enable = true;
                  # OPTIONAL customize font set, e.g., filtering all jinkai fonts.
                  fonts =
                    with lib;
                    with pkgs.tsangertypeFonts;
                    let
                      pname = drv: attrByPath [ "pname" ] "unknown" drv;
                      cond = drv: hasPrefix "tsangertype-jinkai" (pname drv);
                    in
                    (listOfFontsWithCond gratisProPersona cond);

                  # OPTIONAL exclude some fonts, e.g., exclude two jinkai fonts.
                  excludedFonts = with pkgs.tsangertypeFonts; [
                    tsangertype-jinkai-02-w01-font
                    tsangertype-jinkai-02-w02-font
                  ];
                };
              };
            }
          )
        ];
      };
    };
}
```

## Options

### `config.fonts.tsangertype-fonts.enable`

Enable TsangerType fonts.

### `config.fonts.tsangertype-fonts.excludedFonts`

A list of fonts to be excluded.

### `config.fonts.tsangertype-fonts.fonts`

List of desired fonts. You can use the scaffolds provided by the [proprius-fonts overlay] to generate this list. By default, it includes all gratis propria persona fonts.

[proprius-fonts overlay]: ../overlays/proprius-fonts.md
