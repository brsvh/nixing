# homeModules.fonts

## Usage

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    brsvh.url = "github:brsvh/nixing";
    brsvh.inputs.nixpkgs.follows = "nixpkgs";
  };

  # OPTIONAL use my cachix binary cache.
  nixConfig = {
    extra-substituters = [ "https://brsvh.cachix.org" ];
    extra-trusted-public-keys = [ "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE=" ];
  };

  outputs =
    { home-manager, ... }@inputs:
    {
      homeConfiguraitons.YOUR-CONFIGURATION = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        modules = [
          brsvh.homeModules.fonts
          (
            { pkgs, ... }:
            {
              fonts = {
                fontconfig = {
                  enable = true;
                  english.enable = true;
                  chinese = {
                    enable = true;

                    defaultFont = {
                      sansSerif = "Source Han Sans SC";
                      serif = "Source Han Serif SC";
                      monospace = "Source Han Mono SC";
                    };

                    defaultFonts = {
                      sansSerif = [
                        "Source Han Sans SC"
                        "Source Han Sans HC"
                        "Source Han Sans TC"
                      ];

                      serif = [
                        "Source Han Serif SC"
                        "Source Han Serif HC"
                        "Source Han Serif TC"
                      ];

                      monospace = [
                        "Source Han Mono SC"
                        "Source Han Mono HC"
                        "Source Han Mono TC"
                      ];
                    };

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

### `config.fonts.fontconfig.<language>.configText`

The content of the fontconfig configuration file for the `<language>` code.

### `config.fonts.fontconfig.<language>.defaultFont.sansSerif`

The default sans serif font for the `<language>` code.

### `config.fonts.fontconfig.<language>.defaultFont.serif`

The default serif font for the `<language>` code.

### `config.fonts.fontconfig.<language>.defaultFont.monospace`

The default monospace font for the `<language>` code.

### `config.fonts.fontconfig.<language>.defaultFonts.sansSerif`

The list of sans serif fonts used for the `<language>` code, with
display priority corresponding to their position in the list.

### `config.fonts.fontconfig.<language>.defaultFonts.serif`

The list of serif fonts used for the `<language>` code, with display
priority corresponding to their position in the list.

### `config.fonts.fontconfig.<language>.defaultFonts.monospace`

The list of monospace fonts used for the `<language>` code, with display
priority corresponding to their position in the list.

### `config.fonts.fontconfig.<language>.enable`

Generate and install the fontconfig configuration file for the `<language>` code.

### `config.fonts.fontconfig.<language>.fonts`

Fonts depended on by `<lanuage>`.
