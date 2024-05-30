{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      # development
      jq

      # network
      curl

      # search
      agrep
      fd
      findutils
      gnugrep
      ripgrep
    ];
  };

  programs = {
    command-not-found = {
      enable = true;
    };

    git = {
      enable = true;

      lfs = {
        enable = true;
      };
    };

    nix-ld = {
      enable = true;
    };
  };
}
