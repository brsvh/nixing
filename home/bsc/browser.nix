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

            "Others" = {
              icon = "fence";
              color = "toolbar";
              id = 5;
            };
          };

          search = {
            default = "Google";

            engines = {
              "GitHub" = {
                definedAliases = [ "@github" ];

                urls =
                  [
                    {
                      template = "https://github.com/search";
                      parmas =
                        [
                          {
                            name = "q";
                            value = "{searchTerms}";
                          }
                        ];
                    }
                  ];

                iconUpdateURL = "https://github.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
              };

              "GitHub Code" = {
                definedAliases = [ "@github-code" ];

                urls =
                  [
                    {
                      template = "https://github.com/search";
                      parmas =
                        [
                          {
                            name = "q";
                            value = "{searchTerms}";
                          }
                          {
                            name = "type";
                            value = "code";
                          }
                        ];
                    }
                  ];

                iconUpdateURL = "https://github.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
              };

              "GitHub Repositories" = {
                definedAliases = [ "@github-repo" ];

                urls =
                  [
                    {
                      template = "https://github.com/search";
                      parmas =
                        [
                          {
                            name = "q";
                            value = "{searchTerms}";
                          }
                          {
                            name = "type";
                            value = "repositories";
                          }
                        ];
                    }
                  ];

                iconUpdateURL = "https://github.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
              };

              "GitLab" = {
                definedAliases = [ "@gitlab" ];

                urls =
                  [
                    {
                      template = "https://gitlab.com/search";
                      parmas =
                        [
                          {
                            name = "search";
                            value = "{searchTerms}";
                          }
                        ];
                    }
                  ];

                iconUpdateURL = "https://gitlab.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
              };

              "GitLab Projects" = {
                definedAliases = [ "@gitlab-projects" ];

                urls =
                  [
                    {
                      template = "https://gitlab.com/search";
                      parmas =
                        [
                          {
                            name = "search";
                            value = "{searchTerms}";
                          }
                          {
                            name = "scope";
                            value = "projects";
                          }
                        ];
                    }
                  ];

                iconUpdateURL = "https://gitlab.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
              };

              "NixOS Options" = {
                urls =
                  [
                    {
                      template = "https://search.nixos.org/packages";
                      params = [
                        {
                          name = "type";
                          value = "options";
                        }
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nixopts" ];
              };

              "Nix Packages" = {
                definedAliases = [ "@nixpkgs" ];

                urls =
                  [
                    {
                      template = "https://search.nixos.org/packages";
                      params = [
                        {
                          name = "type";
                          value = "packages";
                        }
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
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
