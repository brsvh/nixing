{ inputs, ... }:
let
  inherit (inputs) sops;
in
{
  home-manager = {
    sharedModules = [ sops.homeManagerModules.sops ];
  };
}
