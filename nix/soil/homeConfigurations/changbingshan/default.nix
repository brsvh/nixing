{
  cell,
  config,
  inputs,
  ...
}:
let
  inherit (inputs.cells) my-emacs unfree;

  system = "x86_64-linux";

  username = "changbingshan";

  fullname = "Bingshan Chang";
in
{
  imports = [
    cell.homeModules.fonts
    cell.homeProfiles.chinese
    cell.homeProfiles.direnv
    cell.homeProfiles.english
    cell.homeProfiles.fish
    cell.homeProfiles.git
    cell.homeProfiles.gnome
    cell.homeProfiles.gnupg
    cell.homeProfiles.google-chrome
    cell.homeProfiles.japanese
    cell.homeProfiles.korean
    cell.homeProfiles.my-emacs
    cell.homeProfiles.obs-studio
    cell.homeProfiles.ssh
    cell.homeProfiles.texlive
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

  home = {
    inherit username;

    homeDirectory = "/home/${username}";

    packages = with pkgs; [ wemeet ];

    stateVersion = "24.05";
  };

  programs = {
    git = {
      signing = {
        key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
        signByDefault = true;
      };

      userEmail = "changbingshan@iscas.ac.cn";
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
