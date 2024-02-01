{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  user = config.home.username;
in
{
  fonts = {
    emoji = {
      enable = true;
      flavour = "Noto";
    };

    chinese = {
      enable = true;
      flavour = "Noto";
      variant = "SC";
    };

    english = {
      enable = true;
      enableNerdFontIntegration = true;
      flavour = "Plex";
    };

    japanese = {
      enable = true;
      flavour = "Noto";
    };

    korean = {
      enable = true;
      flavour = "Noto";
    };
  };

  xdg = {
    configFile = {
      "fontconfig/conf.d/99-${user}-fonts.conf".text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
          <its:rules xmlns:its="http://www.w3.org/2005/11/its" version="1.0">
            <its:translateRule
                translate="no"
                selector="/fontconfig/*[not(self::description)]"
                />
          </its:rules>

          <description>"${user}" Font Config</description>

          <match target="font">
            <edit name="embeddedbitmap" mode="assign">
              <bool>false</bool>
            </edit>
          </match>

          <config>
            <!-- Rescan configuration every 30 seconds when FcFontSetList is called -->
            <rescan>
              <int>30</int>
            </rescan>
          </config>
        </fontconfig>
      '';
    };
  };
}
