# nixosModules.fonts

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
          brsvh.nixosModules.fonts
          (
            { pkgs, ... }:
            {
              fonts = {
                fontconfig = {
                  enable = true;
                  english.enable = true;
                  chinese = {
                    enable = true;
                    monospace = "Source Han Mono SC";
                    sansSerif = "Source Han Sans SC";
                    serif = "Source Han Serif SC";

                    fonts = with pkgs; [
                      source-han-sans
                      source-han-serif
                      source-han-mono
                    ];
                  };
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

Supported `<lanuage>` includes:

- `english`, for `en` language code.
- `chinese`, for `zh` language code.
- `japanese`, for `ja` language code.
- `korean`, for `ko` language code.

### `config.fonts.fontconfig.<language>.sansSerif`

The default sans serif font for the `<language>` code.

### `config.fonts.fontconfig.<language>.serif`

The default serif font for the `<language>` code.

### `config.fonts.fontconfig.<language>.monospace`

The default monospace font for the `<language>` code.

### `config.fonts.fontconfig.<language>.enable`

Generate and install the fontconfig configuration file for the `<language>` code.

### `config.fonts.fontconfig.<language>.fonts`

Fonts depended on by `<lanuage>`.

### `config.fonts.fontconfig.emoji.defaultFont`

The default font used for the emoji.


### `config.fonts.fontconfig.symbol.defaultFont`

The default font used for the symbol.
