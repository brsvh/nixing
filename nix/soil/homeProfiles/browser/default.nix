{ config, pkgs, ... }:
{
  programs = {
    firefox = {
      enable = true;

      profiles = {
        "${config.home.username}" = {
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

            "Others" = {
              icon = "fence";
              color = "toolbar";
              id = 5;
            };
          };

          search = {
            default = "Google";

            engines = {
              "NixOS Options" = {
                urls = [
                  {
                    template = "https://search.nixos.org/options";
                    params = [
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                      {
                        name = "channel";
                        value = "unstable";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nixopts" ];
              };

              "Nix Packages" = {
                definedAliases = [ "@nixpkgs" ];

                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                      {
                        name = "channel";
                        value = "unstable";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
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
