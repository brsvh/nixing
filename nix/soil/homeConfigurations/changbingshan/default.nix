{
  cell,
  inputs,
  ...
}:
let
  inherit (inputs) nix-alien;

  inherit (inputs.cells) apps fonts my-emacs;

  system = "x86_64-linux";
in
{
  imports = [
    cell.homeUsers.changbingshan
  ];

  bee = {
    inherit system;

    home = inputs.home-manager-unstable;

    pkgs = import inputs.nixpkgs-unstable {
      inherit system;

      config = {
        allowUnfree = true;
      };

      overlays = [
        apps.overlays.unfree
        fonts.overlays.proprius-fonts
        my-emacs.overlays.emacs
        nix-alien.overlays.default
      ];
    };
  };
}
