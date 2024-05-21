{ cell, config, ... }:
let
  system = "x86_64-linux";
in
{
  imports = [
    cell.homeProfiles.fish
    cell.homeProfiles.gnupg
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
    };
  };

  home =
    let
      username = "changbingshan";
    in
    {
      inherit username;

      homeDirectory = "/home/${username}";

      packages = [ cell.packages.wemeet ];

      stateVersion = "24.05";
    };
}
