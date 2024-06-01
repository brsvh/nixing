{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      onlyoffice-bin_latest
      wpsoffice
    ];
  };
}
