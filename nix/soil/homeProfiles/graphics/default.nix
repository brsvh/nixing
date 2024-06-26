{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      gimp-with-plugins
      graphviz
      imagemagick
      inkscape-with-extensions
    ];
  };
}
