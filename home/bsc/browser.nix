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
  programs = {
    firefox = {
      enable = true;

      profiles = {
        "${user}" = {
          name = config.home.fullname;
          id = 0;

          containers = {
            "Personal" = {
              icon = "fingerprint";
              color = "blue";
              id = 0;
            };

            "Work" = {
              color = "orange";
              icon = "briefcase";
              id = 1;
            };

            "Banking" = {
              icon = "dollar";
              color = "green";
              id = 2;
            };

            "Shopping" = {
              icon = "cart";
              color = "pink";
              id = 3;
            };

            "FLOSS" = {
              icon = "tree";
              color = "purple";
              id = 4;
            };
          };

          search = {
            default = "Google";

            engines = {
              "Nix Packages" = {
                urls =
                  [
                    {
                      template = "https://search.nixos.org/packages";
                      params = [
                        { name = "type"; value = "packages"; }
                        { name = "query"; value = "{searchTerms}"; }
                      ];
                    }
                  ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nixpkgs" ];
              };

              "NixOS Options" = {
                urls =
                  [
                    {
                      template = "https://search.nixos.org/options?type=options&channel=unstable&query={searchTerms}";
                    }
                  ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nixopts" ];
              };

              "Amazon".metaData.hidden = true;
              "eBay".metaData.hidden = true;
            };
          };

          settings = {
            "app.update.auto" = false;
            "browser.bookmarks.showMobileBookmarks" = true;
          };
        };
      };
    };
  };
}
