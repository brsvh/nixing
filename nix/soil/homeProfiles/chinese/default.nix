{
  cell,
  config,
  inputs,
  pkgs,
  ...
}:
let
  inherit (inputs.lib) generators;
in
{
  imports = [
    cell.homeProfiles.fonts
    cell.homeProfiles.rime
  ];

  fonts = {
    fontconfig = {
      chinese = {
        defaultFont = {
          sansSerif = "Source Han Sans SC";
          serif = "Source Han Serif SC";
          monospace = "Source Han Mono SC";
        };

        defaultFonts = {
          sansSerif = [
            "Source Han Sans SC"
            "Source Han Sans HC"
            "Source Han Sans TC"
          ];

          serif = [
            "Source Han Serif SC"
            "Source Han Serif HC"
            "Source Han Serif TC"
          ];

          monospace = [
            "Source Han Mono SC"
            "Source Han Mono HC"
            "Source Han Mono TC"
          ];
        };

        enable = true;

        fonts = with pkgs; [
          source-han-sans
          source-han-serif
          source-han-mono
        ];
      };
    };
  };

  i18n = {
    inputMethod = {
      enabled = "fcitx5";
    };
  };

  xdg = {
    configFile = {
      "fcitx5/config".source = pkgs.writeText "fcitx5_config.conf" (
        generators.toINI { } {
          Hotkey = {
            EnumerateWithTriggerKeys = true;
            EnumerateForwardKeys = null;
            EnumerateBackwardKeys = null;
            EnumerateSkipFirst = null;
          };

          "Hotkey/TriggerKeys" = {
            "0" = "Super+space";
            "1" = "Zenkaku_Hankaku";
            "2" = "Hangul";
          };

          "Hotkey/AltTriggerKeys" = {
            "0" = "Shift_L";
          };

          "Hotkey/ActivateKeys" = {
            "0" = "Hangul_Hanja";
          };

          "Hotkey/DeactivateKeys" = {
            "0" = "Hangul_Romaja";
          };

          "Hotkey/PrevPage" = {
            "0" = "Up";
          };

          "Hotkey/NextPage" = {
            "0" = "Down";
          };

          "Hotkey/PrevCandidate" = {
            "0" = "Shift+Tab";
          };

          "Hotkey/NextCandidate" = {
            "0" = "Tab";
          };

          "Hotkey/TogglePreedit" = {
            "0" = "Control+Alt+P";
          };

          Behavior = {
            AllowInputMethodForPassword = false;
            AutoSavePeriod = "30";
            CompactInputmethodInformation = true;
            CustomXkbOption = null;
            DefaultPageSize = "8";
            DisabledAddons = null;
            EnabledAddons = null;
            OverriedXkbOption = false;
            PreeditEnabledByDefault = true;
            PreloadInputMethod = true;
            ShareInputState = "No";
            ShowFirstInputMethodInformation = true;
            ShowInputMethodInformation = true;
            ShowInputMethodInformationWhenFocusIn = false;
            ShowPreeditForPassword = false;
            ActiveByDefault = true;
          };
        }
      );

      "fcitx5/profile".source = pkgs.writeText "fcitx5_profie.conf" (
        generators.toINI { } {
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = "us";
            DefaultIM = "rime";
          };

          "Groups/0/Items/0" = {
            Name = "rime";
            Layout = null;
          };

          "GroupOrder" = {
            "0" = "Default";
          };
        }
      );

      "fcitx5/conf/classicui.conf".source = pkgs.writeText "fcitx5_classicui.conf" (
        generators.toINIWithGlobalSection { } {
          globalSection = {
            "Vertical Candidate List" = false;
            DarkTheme = "default-dark";
            EnableFractionalScale = true;
            Font = "${config.fonts.fontconfig.chinese.defaultFont.serif} 11";
            ForceWaylandDPI = 0;
            MenuFont = "${config.fonts.fontconfig.chinese.defaultFont.serif} 11";
            PerScreenDPI = true;
            PreferTextIcon = false;
            ShowLayoutNameInIcon = true;
            Theme = "default";
            TrayFont = "${config.fonts.fontconfig.chinese.defaultFont.sansSerif} 11";
            TrayOutlineColor = "#000000";
            TrayTextColor = "#ffffff";
            UseAccentColor = true;
            UseDarkTheme = true;
            UseInputMethodLanguageToDisplayText = true;
            WheelForPaging = true;
          };
        }
      );
    };
  };
}
