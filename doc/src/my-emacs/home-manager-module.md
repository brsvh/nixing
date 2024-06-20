# homeModules.my-emacs

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
          brsvh.homeModules.my-emacs
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                config.allowUnfree = true;
                overlays = [ brsvh.overlays.emacs ];
              };
              programs = {
                my-emacs = {
                  enable = true;
                  variant = "pgtk"; # Use pgtk variant.
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

## Options

### `programs.my-emacs.defaultEditor`

Configures emacsclient to be the default editor using the EDITOR
environment variable.

### `programs.my-emacs.enable`

Whether use `my-emacs`.

### `programs.my-emacs.extraBinaries`

Extra executable binaries will be add to `my-emacs`.

### `programs.my-emacs.extraConfig`

Extra configuration will be add to `my-emacs`.

### `programs.my-emacs.extraEmacsPackages`

Extra Emacs Lisp packages will be add to `my-emacs`.

### `programs.my-emacs.extraFonts`

Extra font packages will be add to `my-emacs`.

### `programs.my-emacs.extraLibraries`

Extra library packages will be add to `my-emacs`.

### `programs.my-emacs.scope`

`my-emacs` package scope will be used.

### `programs.my-emacs.userMail`

Customize `user-mail-address` of GNU Emacs.

### `programs.my-emacs.userName`

Customize `user-full-name` of GNU Emacs.

### `programs.my-emacs.variant`

Which variant of `my-emacs` to use.
