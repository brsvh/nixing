{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  home = {
    packages = with pkgs;
      [
        foundertype-fonts
      ];
  };

  programs = {
    texlive = {
      enable = true;
      extraPackages = tpkgs:
        { inherit (tpkgs) scheme-full; };
    };
  };
}
