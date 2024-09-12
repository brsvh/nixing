{ lib, ... }:
{
  nix = {
    gc = {
      automatic = false;
      dates = "weekly";

      options = lib.mkDefault ''
        --delete-older-than 4w
      '';
    };

    optimise = {
      automatic = true;
    };

    settings = {
      allowed-users = [ "@users" ];

      builders-use-substitutes = true;

      experimental-features = [
        "ca-derivations"
        "flakes"
        "nix-command"
      ];

      fallback = true;

      keep-derivations = lib.mkDefault true;

      keep-outputs = lib.mkDefault true;

      sandbox = true;

      trusted-users = [
        "@wheel"
        "root"
      ];

      use-xdg-base-directories = true;
    };
  };

  programs = {
    nh = {
      clean = {
        enable = true;
        extraArgs = "--keep-since 4w --keep 10";
      };

      enable = true;
    };
  };
}
