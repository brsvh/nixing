{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [ microsoft-edge ];

    sessionVariables = {
      GOOGLE_DEFAULT_CLIENT_ID = "699382231816-ubaprol9esc9qpeahi2o31iftf2tn9pv.apps.googleusercontent.com";
      GOOGLE_DEFAULT_CLIENT_SECRET = "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";
    };
  };

  programs = {
    chromium = {
      enable = true;
      commandLineArgs = [
        # FIXME uncomment when fcitx5 is work
        # "--enable-features=UseOzonePlatform"
        # "--gtk-version=4"
        # "--ozone-platform=wayland"
      ];
    };
  };
}
