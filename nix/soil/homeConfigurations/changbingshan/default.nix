{
  cell,
  config,
  inputs,
  ...
}:
let
  inherit (inputs.cells) my-emacs unfree;

  system = "x86_64-linux";
in
{
  imports = [
    cell.homeProfiles.chinese
    cell.homeProfiles.direnv
    cell.homeProfiles.english
    cell.homeProfiles.fish
    cell.homeProfiles.gnome
    cell.homeProfiles.gnupg
    cell.homeProfiles.google-chrome
    cell.homeProfiles.japanese
    cell.homeProfiles.korean
    cell.homeProfiles.my-emacs
    cell.homeProfiles.xdg
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
        my-emacs.overlays.emacs
        unfree.overlays.unfree
      ];
    };
  };

  home =
    let
      username = "changbingshan";
    in
    {
      inherit username;

      homeDirectory = "/home/${username}";

      packages = with pkgs; [ wemeet ];

      stateVersion = "24.05";
    };
}
