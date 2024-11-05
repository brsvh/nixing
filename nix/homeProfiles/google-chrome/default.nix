{
  ...
}:
{
  programs = {
    google-chrome = {
      commandLineArgs = [
        "--enable-features=UseOzonePlatform"
        "--ozone-platform=wayland"
        "--enable-wayland-ime"
        "--wayland-text-input-version=3"
      ];

      enable = true;
    };
  };
}
