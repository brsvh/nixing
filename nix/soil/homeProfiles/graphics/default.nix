{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      # REVIEW uncomment when it is fixed
      # gimp-with-plugins
      graphviz
      imagemagick
      inkscape-with-extensions
    ];
  };
}
