# Nixing

A place to collect things about Nix, it is also where to keep my
dotfiles[^1].

## Overview

This project offers [Nix] packages maintained by myself and their [Nixpkgs]
overlays, extended [NixOS] modules and [Home Manager] modules.

[Home Manager]: https://github.com/nix-community/home-manager
[NixOS]: https://nixos.org
[Nix]: https://github.com/NixOS/nix
[Nixpkgs]: https://github.com/NixOS/nixpkgs

Additionally, this project retains my NixOS host configuration, Home
Manager user configuration, disk layout configuration with [disko], and
deployment configuration with [colmena].

[disko]: https://github.com/nix-community/disko
[colmena]: https://github.com/zhaofengli/colmena

I use [std] and [hive] to support the continuous growth of the code in
this repository. All packages, modules, and overlays keep across
different subdirectories (called `cell`) under the nix directory (called
`cellBlock`). You can read the [paisano] documentation to understand the
details and principles of code organization.

[hive]: https://github.com/divnix/hive
[paisano]: https://github.com/paisano-nix/core
[std]: https://github.com/divnix/std

### Nixpkgs overlays

This repository provides the following overlays.

- `emacs`, offers my portable GNU Emacs configurations.
- `proprius-fonts`, offers some proprietary fonts.
- `unfree`, provides some non-free software.

### NixOS modules

This repository provides the following NixOS modules.

- `fonts`, helps generate optimized fontconfig for languages, currently
  including English, Chinese, Japanese, and Korean.
- `my-emacs`, provides customized support for my portable GNU Emacs
  configurations on NixOS.
- `tsangertype-fonts`, Supports the installation of all TsangerType fonts or a subset of them.

### Home Manager modules

Currently, the Home Manager modules provided by this repository align in
functionality and naming with the NixOS modules.

## Getting Started

I recommend use Nix Flake to import my packages, modules, and overlays,
as I have not tested other methods.

Firstly, add my repository to `inputs`.

``` nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/unstable";

  inputs.brsvh.url = "github:brsvh/nixing";
  inputs.brsvh.inputs.nixpkgs.follows = "nixpkgs";

  # OPTIONAL use my cachix binary cache.
  nixConfig.extra-substituters = [ "https://brsvh.cachix.org" ];
  nixConfig.extra-trusted-public-keys = [ "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE="];
}
```

Then, import and use anything what you need.

``` nix
{
  outputs.nixosConfiguraitons.YOUR-CONFIGURATION = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      brsvh.nixosModules.fonts
      brsvh.nixosModules.tsangertype-fonts
      (
        { pkgs, ... }:
        {
          nixpkgs.config.allowUnfree = true;
          nixpkgs.overlays = [
              brsvh.overlays.emacs
              brsvh.overlays.proprius-fonts
              brsvh.overlays.unfree
            ];

          environment.systemPackageswith = with pkgs; [ wemeet ];

	  # This option from `brsvh.nixosModules.fonts`.
	  fonts.fontconfig.chinese.enable = true;

	  # This option from `brsvh.nixosModules.tsangertype-fonts`.
	  fonts.tsangertype-fonts.enable = true;
        }
      )
    ];
  };
}
```

## License

Unless otherwise specified, all work is free. You can redistribute it
and/or modify it under the terms of the Do What The Fuck You Want To
Public License, Version 2, as published by Sam Hocevar. You should have
received a copy of it, see the *COPYING* file for more details. If
you did not recive it, see <http://www.wtfpl.net> for more details.

## Thanks

Thanks to these software in the Nix ecosystem.

- [disko](https://github.com/nix-community/disko)
- [Hive](https://github.com/divnix/hive)
- [NixOS hardware](https://github.com/NixOS/nixos-hardware)
- [Standard](https://std.divnix.com/)
- [colmena](https://colmena.cli.rs)

[^1]: What is dotfiles, <https://dotfiles.github.io>
