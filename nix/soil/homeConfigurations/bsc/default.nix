{
  cell,
  config,
  inputs,
  ...
}:
let
  inherit (inputs.cells) apps fonts my-emacs;

  system = "x86_64-linux";

  username = "bsc";

  fullname = "Burgess Chang";
in
{
  imports = [
    cell.homeProfiles.browser
    cell.homeProfiles.cachix
    cell.homeProfiles.chinese
    cell.homeProfiles.direnv
    cell.homeProfiles.english
    cell.homeProfiles.fish
    cell.homeProfiles.git
    cell.homeProfiles.gnome
    cell.homeProfiles.gnupg
    cell.homeProfiles.graphics
    cell.homeProfiles.japanese
    cell.homeProfiles.korean
    cell.homeProfiles.modules
    cell.homeProfiles.my-emacs
    cell.homeProfiles.node
    cell.homeProfiles.obs-studio
    cell.homeProfiles.ssh
    cell.homeProfiles.texlive
    cell.homeProfiles.tools
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
        fonts.overlays.proprius-fonts
        my-emacs.overlays.emacs
        apps.overlays.unfree
      ];
    };
  };

  home = {
    inherit username;

    homeDirectory = "/home/${username}";

    packages = with pkgs; [ wemeet ];

    stateVersion = "24.11";
  };

  programs = {
    git = {
      signing = {
        key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
        signByDefault = true;
      };

      userEmail = "bsc@brsvh.org";
      userName = fullname;
    };

    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
      };
    };
  };
}
