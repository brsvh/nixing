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

  home = {
    packages = with pkgs;
      [
        tsangertype-font-jinkai01-w01
        tsangertype-font-jinkai01-w02
        tsangertype-font-jinkai01-w03
        tsangertype-font-jinkai01-w04
        tsangertype-font-jinkai01-w05
        tsangertype-font-jinkai02-w01
        tsangertype-font-jinkai02-w02
        tsangertype-font-jinkai02-w03
        tsangertype-font-jinkai02-w04
        tsangertype-font-jinkai02-w05
        tsangertype-font-jinkai03-w01
        tsangertype-font-jinkai03-w02
        tsangertype-font-jinkai03-w03
        tsangertype-font-jinkai03-w04
        tsangertype-font-jinkai03-w05
        tsangertype-font-jinkai04-w01
        tsangertype-font-jinkai04-w02
        tsangertype-font-jinkai04-w03
        tsangertype-font-jinkai04-w04
        tsangertype-font-jinkai04-w05
        tsangertype-font-jinkai05-w01
        tsangertype-font-jinkai05-w02
        tsangertype-font-jinkai05-w03
        tsangertype-font-jinkai05-w04
        tsangertype-font-jinkai05-w05
        tsangertype-font-minghei-w01
        tsangertype-font-minghei-w02
        tsangertype-font-minghei-w03
        tsangertype-font-minghei-w04
        tsangertype-font-minghei-w05
        tsangertype-font-minghei-w06
        tsangertype-font-minghei-w07
        tsangertype-font-minghei-w08
        tsangertype-font-mingkai-w01
        tsangertype-font-mingkai-w02
        tsangertype-font-mingkai-w03
        tsangertype-font-mingkai-w04
        tsangertype-font-xuansan01-w01
        tsangertype-font-xuansan01-w02
        tsangertype-font-xuansan01-w03
        tsangertype-font-xuansan01-w04
        tsangertype-font-xuansan01-w05
        tsangertype-font-xuansan02-w01
        tsangertype-font-xuansan02-w02
        tsangertype-font-xuansan02-w03
        tsangertype-font-xuansan02-w04
        tsangertype-font-xuansan02-w05
        tsangertype-font-xuansan03-w01
        tsangertype-font-xuansan03-w02
        tsangertype-font-xuansan03-w03
        tsangertype-font-xuansan03-w04
        tsangertype-font-xuansan03-w05
        tsangertype-font-xuansan04-w01
        tsangertype-font-xuansan04-w02
        tsangertype-font-xuansan04-w03
        tsangertype-font-xuansan04-w04
        tsangertype-font-xuansan04-w05
        tsangertype-font-xuansanm-w01
        tsangertype-font-xuansanm-w02
        tsangertype-font-xuansanm-w03
        tsangertype-font-xuansanm-w04
        tsangertype-font-xuansanm-w05
        tsangertype-font-yunhei-w01
        tsangertype-font-yunhei-w02
        tsangertype-font-yunhei-w03
        tsangertype-font-yunhei-w04
        tsangertype-font-yunhei-w05
        tsangertype-font-yunhei-w06
        tsangertype-font-yunhei-w07
        tsangertype-font-yunhei-w08
      ];
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
