{
  cell,
  config,
  inputs,
  pkgs,
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
    cell.homeProfiles.email
    cell.homeProfiles.english
    cell.homeProfiles.fish
    cell.homeProfiles.git
    cell.homeProfiles.plasma
    cell.homeProfiles.gnupg
    cell.homeProfiles.graphics
    cell.homeProfiles.japanese
    cell.homeProfiles.korean
    cell.homeProfiles.modules
    cell.homeProfiles.my-emacs
    cell.homeProfiles.node
    cell.homeProfiles.obs-studio
    cell.homeProfiles.password
    cell.homeProfiles.ssh
    cell.homeProfiles.texlive
    cell.homeProfiles.tools
    cell.homeProfiles.xdg
    cell.homeSecrets.bsc
  ];

  accounts = {
    email = {
      accounts = {
        "${username}" = {
          address = "bsc@brsvh.org";

          aliases = [
            "open@brsvh.org"
            "register@brsvh.org"
          ];

          gpg = {
            key = "78D74502D92E0218";
            signByDefault = true;
          };

          imap = {
            host = "imappro.zoho.com";
            port = 993;

            tls = {
              enable = true;
            };
          };

          smtp = {
            host = "smtppro.zoho.com";
            port = 465;

            tls = {
              enable = true;
            };
          };

          realName = fullname;
          userName = "bsc@brsvh.org";
        };
      };
    };
  };

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

    packages = with pkgs; [
      feishu
      wemeet
    ];

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
