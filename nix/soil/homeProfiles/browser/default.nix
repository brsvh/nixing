{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [ microsoft-edge ];
  };

  programs = {
    chromium = {
      enable = true;
      commandLineArgs = [
        "--enable-features=UseOzonePlatform"
        "--gtk-version=4"
        "--ozone-platform=wayland"
      ];
    };
  };
}
