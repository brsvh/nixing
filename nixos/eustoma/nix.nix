{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
with lib;
let
  scrt = config.sops.secrets;
in
{
  nix = {
    gc = {
      automatic = true;
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

      experimental-features = [
        "ca-derivations"
        "flakes"
        "nix-command"
        "repl-flake"
      ];

      sandbox = true;

      trusted-users = [
        "@admin"
        "@wheel"
        "root"
      ];

      use-xdg-base-directories = true;
    };
  };

  services = {
    hercules-ci-agent = {
      enable = true;
      settings = {
        binaryCachesPath = scrt."hercules-ci/binary-caches.json".path;
        clusterJoinTokenPath = scrt."hercules-ci/cluster-join-token.key".path;
        concurrentTasks = 4;
      };
    };
  };
}
