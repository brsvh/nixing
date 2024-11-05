{
  lib,
  ...
}:
let
  inherit (lib)
    mkDefault
    ;
in
{
  nix = {
    gc = {
      automatic = false;
      dates = "weekly";

      options = mkDefault ''
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

      keep-derivations = mkDefault true;

      keep-outputs = mkDefault true;

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
