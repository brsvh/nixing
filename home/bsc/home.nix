{ config
, home-manager
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  currentUser = config.home.username;
  currentEMail = config.accounts.email.accounts."${currentUser}";
  currentHost = elemAt (strings.split "@" currentEMail.address) 2;
in
{
  accounts = {
    email = {
      accounts = {
        "${currentUser}" = {
          primary = true;
          realName = "Burgess Chang";
          address = "bsc@brsvh.org";
          userName = "bsc@brsvh.org";
          passwordCommand =
            "pass ${currentHost}/mail/${currentEMail.userName}";

          aliases =
            [
              "open@brsvh.org"
              "register@brsvh.org"
            ];

          gpg = {
            key = "78D74502D92E0218";
            signByDefault = true;
          };

          imap = {
            host = "imappro.zoho.com";
            port = 993;
            tls = {
              enable = true;
            };
          };

          smtp = {
            host = "smtppro.zoho.com";
            port = 465;
            tls = {
              enable = true;
            };
          };

          offlineimap = {
            enable = true;
            extraConfig = {
              account = {
                autorefresh = 5;
                quick = 10;
              };

              local = {
                sync_deletes = true;
              };

              remote = {
                maxconnections = 2;
              };
            };
          };

          mu = {
            enable = true;
          };

          thunderbird = {
            enable = false;
            profiles = [ "${currentUser}" ];
          };
        };
      };

      maildirBasePath = "${config.xdg.dataHome}/Mail/";
    };
  };

  emacs.d = {
    enable = true;
    platform = "wayland";

    extraInitConfig = ''
      (setq mail-host-address "${currentHost}")

      (use-package mail-source
        :config
        (setq mail-source-directory
              "${config.accounts.email.maildirBasePath}"))

      (use-package smtpmail
        :config
        (setq
         send-mail-function 'smtpmail-send-it
         smtpmail-smtp-server "${currentEMail.smtp.host}"
         smtpmail-smtp-service "${toString currentEMail.smtp.port}"
         smtpmail-stream-type ${
           if currentEMail.smtp.tls.enable
           then "'ssl"
           else "nil"
         }))
    '';

    overrides = {
      mu4e = _: _: {
        src = pkgs.mu.mu4e;
      };
    };
  };

  fonts = {
    emoji = {
      enable = true;
      flavour = "Noto";
    };

    english = {
      enable = true;
      enableNerdFontIntegration = true;
      flavour = "Source";
    };

    chinese = {
      enable = true;
      flavour = "Source";
      variant = "SC";
    };
  };

  gtk = {
    enable = true;

    cursorTheme = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      size = 16;
    };

    gtk2 = {
      configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    };
  };

  home = {
    packages = with pkgs;
      [
        cachix
        foundertype-fonts
      ];

    sessionPath =
      [
        "${config.home.sessionVariables.XDG_BIN_HOME}"
      ];

    sessionVariables = {
      XDG_BIN_HOME = "${config.home.homeDirectory}/.local/bin";
    };
  };

  nix = {
    package = pkgs.nixUnstable;
    settings = {
      experimental-features =
        [
          "ca-derivations"
          "flakes"
          "nix-command"
          "repl-flake"
        ];
      use-xdg-base-directories = true;
    };
  };

  programs = {
    any-nix-shell = {
      enable = true;
      enableFishIntegration = true;
    };

    firefox = {
      enable = true;
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
      '';
    };

    git = {
      enable = true;
      signing = {
        key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
        signByDefault = true;
      };
      userEmail = "bsc@brsvh.org";
      userName = "Burgess Chang";
    };

    gpg = {
      enable = true;
      homedir = "${config.xdg.stateHome}/gnupg";
    };

    home-manager = {
      enable = true;
      path = mkForce "${home-manager}";
    };

    offlineimap = {
      enable = currentEMail.offlineimap.enable;
    };

    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
      };
    };

    ssh = {
      enable = true;

      includes =
        [
          "isrc/*"
        ];

      matchBlocks = {
        "github.com" = {
          hostname = "ssh.github.com";
          user = "git";
          port = 443;
        };
      };
    };

    starship = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      settings = {
        aws = {
          symbol = " ";
        };

        buf = {
          symbol = " ";
        };

        c = {
          symbol = " ";
        };

        conda = {
          symbol = " ";
        };

        dart = {
          symbol = " ";
        };

        directory = {
          read_only = " 󰌾";
        };

        docker_context = {
          symbol = " ";
        };

        elixir = {
          symbol = " ";
        };

        elm = {
          symbol = " ";
        };

        fossil_branch = {
          symbol = " ";
        };

        git_branch = {
          symbol = " ";
        };

        git_status = {
          format = "[[($conflicted$untracked$modified$staged$renamed$deleted)](218) ($ahead_behind$stashed)]($style) ";
          conflicted = "​󰩋 ";
          untracked = "​󰷊 ";
          modified = "󱇧​ ";
          staged = "​󰈖 ";
          renamed = "​󰪹 ";
          deleted = "​󰮘 ";
          stashed = "󰙰 ";
        };

        git_state = {
          format = "\([ $state ($progress_current/$progress_total) ] ($style)\) ";
          style = "bright-black";
        };

        golang = {
          symbol = " ";
        };

        guix_shell = {
          symbol = " ";
        };

        haskell = {
          symbol = " ";
        };

        haxe = {
          symbol = " ";
        };

        hg_branch = {
          symbol = " ";
        };

        hostname = {
          ssh_symbol = " ";
        };

        java = {
          symbol = " ";
        };

        julia = {
          symbol = " ";
        };

        lua = {
          symbol = " ";
        };

        memory_usage = {
          symbol = "󰍛 ";
        };

        meson = {
          symbol = "󰔷 ";
        };

        nim = {
          symbol = "󰆥 ";
        };

        nix_shell = {
          symbol = " ";
        };

        nodejs = {
          symbol = " ";
        };

        os = {
          disabled = false;

          symbols = {
            Alpaquita = " ";
            Alpine = " ";
            Amazon = " ";
            Android = " ";
            Arch = " ";
            Artix = " ";
            CentOS = " ";
            Debian = " ";
            DragonFly = " ";
            Emscripten = " ";
            EndeavourOS = " ";
            Fedora = " ";
            FreeBSD = " ";
            Garuda = "󰛓 ";
            Gentoo = " ";
            HardenedBSD = "󰞌 ";
            Illumos = "󰈸 ";
            Linux = " ";
            Mabox = " ";
            Macos = " ";
            Manjaro = " ";
            Mariner = " ";
            MidnightBSD = " ";
            Mint = " ";
            NetBSD = " ";
            NixOS = " ";
            OpenBSD = "󰈺 ";
            openSUSE = " ";
            OracleLinux = "󰌷 ";
            Pop = " ";
            Raspbian = " ";
            Redhat = " ";
            RedHatEnterprise = " ";
            Redox = "󰀘 ";
            Solus = "󰠳 ";
            SUSE = " ";
            Ubuntu = " ";
            Unknown = " ";
            Windows = "󰍲 ";
          };
        };

        package = {
          symbol = "󰏗 ";
        };

        pijul_channel = {
          symbol = " ";
        };

        python = {
          symbol = " ";
        };

        rlang = {
          symbol = "󰟔 ";
        };

        ruby = {
          symbol = " ";
        };

        rust = {
          symbol = " ";
        };

        scala = {
          symbol = " ";
        };

        username = {
          show_always = true;
          disabled = false;
        };
      };
    };

    thunderbird = {
      enable = currentEMail.thunderbird.enable;
      profiles = {
        "${currentUser}" = {
          isDefault = true;
          withExternalGnupg = true;
        };
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableBashIntegration = config.programs.bash.enable;
      enableExtraSocket = true;
      enableSshSupport = true;
      enableFishIntegration = config.programs.fish.enable;
      pinentryFlavor = "gnome3";
    };

    offlineimap = {
      enable = currentEMail.offlineimap.enable;
    };
  };

  systemd = {
    user = {
      sessionVariables = config.home.sessionVariables;
    };
  };

  xdg = {
    enable = true;

    userDirs = {
      enable = true;
      createDirectories = true;
      desktop = "${config.xdg.dataHome}/Desktop";
      documents = "${config.xdg.dataHome}/Documents";
      download = "${config.xdg.dataHome}/Downloads";
      music = "${config.xdg.dataHome}/Music";
      pictures = "${config.xdg.dataHome}/Pictures";
      publicShare = "${config.xdg.dataHome}/Public";
      templates = "${config.xdg.dataHome}/Templates";
      videos = "${config.xdg.dataHome}/Videos";
    };

    configFile = {
      "fontconfig/conf.d/99-bsc-fonts.conf".text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
          <its:rules xmlns:its="http://www.w3.org/2005/11/its" version="1.0">
            <its:translateRule
                translate="no"
                selector="/fontconfig/*[not(self::description)]"
                />
          </its:rules>

          <description>BSC Font Config</description>

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

      "ibus/rime/default.custom.yaml".source = ./rime/rime.yaml;
      "ibus/rime/ibus_rime.custom.yaml".source = ./rime/ibus.yaml;
      "ibus/rime/luna_pinyin_simp.custom.yaml".source = ./rime/luna_pinyin_simp.yaml;
    };

    dataFile = {
      "fcitx5/rime/default.custom.yaml".source = ./rime/rime.yaml;
      "fcitx5/rime/ibus_rime.custom.yaml".source = ./rime/ibus.yaml;
      "fcitx5/rime/luna_pinyin_simp.custom.yaml".source = ./rime/luna_pinyin_simp.yaml;
    };
  };
}
