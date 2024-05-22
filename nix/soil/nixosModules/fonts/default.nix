{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
let
  langModule =
    lang: fonts:
    {
      monospace,
      sansSerif,
      serif,
    }@fontNames:
    {
      defaultFont = {
        monospace = lib.mkOption {
          type = with lib.types; str;
          default = monospace;
          description = lib.mdDoc ''
            The default monospace font name of ${lang}.
          '';
        };

        sansSerif = lib.mkOption {
          type = with lib.types; str;
          default = sansSerif;
          description = lib.mdDoc ''
            The default sans font name of ${lang}.
          '';
        };

        serif = lib.mkOption {
          type = with lib.types; str;
          default = serif;
          description = lib.mdDoc ''
            The default serif font name of ${lang}.
          '';
        };
      };

      defaultFonts = {
        monospace = lib.mkOption {
          type = with lib.types; listOf str;
          default = [ monospace ];
          description = lib.mdDoc ''
            Default monospace font(s).
          '';
        };

        sansSerif = lib.mkOption {
          type = with lib.types; listOf str;
          default = [ sansSerif ];
          description = lib.mdDoc ''
            Default sans serif font(s).
          '';
        };

        serif = lib.mkOption {
          type = with lib.types; listOf str;
          default = [ serif ];
          description = lib.mdDoc ''
            Default serif font(s).
          '';
        };
      };

      enable = lib.mkOption {
        type = with lib.types; bool;
        default = false;
        description = lib.mdDoc ''
          Whether to enable ${lang} fontconfig.
        '';
      };

      fonts = lib.mkOption {
        type = with lib.types; listOf package;
        default = fonts;
        description = lib.mdDoc ''
          The fonts packages of ${lang} fontconfig will be used.
        '';
      };
    };

  genPreferConfig =
    fonts: type:
    lib.optionalString (fonts != [ ]) ''
      <alias binding="same">
        <family>${type}</family>
        <prefer>
        ${
          lib.concatStringsSep "" (
            map (font: ''
              <family>${font}</family>
            '') fonts
          )
        }
        </prefer>
      </alias>
    '';

  fontconfigs = {
    chinese =
      let
        inherit (config.fonts.fontconfig.english) defaultFont defaultFonts;
      in
      ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
        <fontconfig>
          <description>Chinese fonts</description>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>zh</string>
            </test>
            <test name="family">
              <string>serif</string>
            </test>
            <edit name="family" mode="prepend">
              <string>${defaultFont.serif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>zh</string>
            </test>
            <test name="family">
              <string>sans-serif</string>
            </test>
            <edit name="family" mode="prepend">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>zh</string>
            </test>
            <test name="family">
              <string>monospace</string>
            </test>
            <edit name="family" mode="prepend">
              <string>${defaultFont.monospace}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>zh</string>
            </test>
            <test qual="any" name="family">
              <string>system-ui</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>zh</string>
            </test>
            <test qual="any" name="family">
              <string>ui-sans-serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test qual="any" name="family">
              <string>WenQuanYi Zen Hei</string>
            </test>
            <edit name="family" mode="assign" binding="same">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test qual="any" name="family">
              <string>WenQuanYi Micro Hei</string>
            </test>
            <edit name="family" mode="assign" binding="same">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test qual="any" name="family">
              <string>WenQuanYi Micro Hei Light</string>
            </test>
            <edit name="family" mode="assign" binding="same">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test qual="any" name="family">
              <string>Microsoft YaHei</string>
            </test>
            <edit name="family" mode="assign" binding="same">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test qual="any" name="family">
              <string>SimHei</string>
            </test>
            <edit name="family" mode="assign" binding="same">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test qual="any" name="family">
              <string>SimSun</string>
            </test>
            <edit name="family" mode="assign" binding="same">
              <string>${defaultFont.serif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test qual="any" name="family">
              <string>SimSun-18030</string>
            </test>
            <edit name="family" mode="assign" binding="same">
              <string>${defaultFont.serif}</string>
            </edit>
          </match>

          ${genPreferConfig defaultFonts.monospace "monospace"}

          ${genPreferConfig defaultFonts.sansSerif "sans-serif"}

          ${genPreferConfig defaultFonts.serif "serif"}

        </fontconfig>
      '';

    emoji =
      let
        inherit (config.fonts.fontconfig.emoji) defaultFont defaultFonts;
      in
      ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
        <fontconfig>
          <description>Emoji fonts</description>

          <match>
            <test qual="any" name="family">
              <string>emoji</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont}</string>
            </edit>
          </match>

          ${genPreferConfig defaultFonts "emoji"}

        </fontconfig>
      '';

    english =
      let
        inherit (config.fonts.fontconfig.english) defaultFont defaultFonts;
      in
      ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
        <fontconfig>
          <description>English fonts</description>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>en</string>
            </test>
            <test qual="any" name="family">
              <string>serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont.serif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>en</string>
            </test>
            <test qual="any" name="family">
              <string>sans-serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>en</string>
            </test>
            <test qual="any" name="family">
              <string>monospace</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont.monospace}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>en</string>
            </test>
            <test qual="any" name="family">
              <string>system-ui</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          <match target="pattern">
            <test name="lang" compare="contains">
              <string>en</string>
            </test>
            <test qual="any" name="family">
              <string>ui-sans-serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>${defaultFont.sansSerif}</string>
            </edit>
          </match>

          ${genPreferConfig defaultFonts.monospace "monospace"}

          ${genPreferConfig defaultFonts.sansSerif "sans-serif"}

          ${genPreferConfig defaultFonts.serif "serif"}

        </fontconfig>
      '';

    symbol =
      let
        inherit (config.fonts.fontconfig.symbol) defaultFont defaultFonts;
      in
      ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
        <fontconfig>
          <description>Symbol fonts</description>

          ${genPreferConfig defaultFonts.monospace "monospace"}

          ${genPreferConfig defaultFonts.sansSerif "sans-serif"}

        </fontconfig>
      '';
  };

  mkFC =
    name: config:
    let
      pname = "fc-51-local-" + name + ".conf";
    in
    pkgs.writeText pname config;

  optionalFC = cond: conf: if cond then conf else pkgs.emptyFile;

  fcPkg =
    let
      cfg = config.fonts.fontconfig;
    in
    with lib;
    pkgs.runCommand "extra-local-fontconfig-conf"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
      }
      ''
        dst=$out/etc/fonts/conf.d
        mkdir -p $dst

        ln -s ${optionalFC cfg.chinese.enable (mkFC "chinese" cfg.chinese.configText)} $dst/51-local-chinese.conf
        ln -s ${optionalFC cfg.emoji.enable (mkFC "chinese" cfg.emoji.configText)} $dst/51-local-emoji.conf
        ln -s ${optionalFC cfg.english.enable (mkFC "chinese" cfg.english.configText)} $dst/51-local-english.conf
        ln -s ${optionalFC cfg.symbol.enable (mkFC "chinese" cfg.symbol.configText)} $dst/51-local-symbol.conf
      '';
in
{
  options.fonts.fontconfig = {
    chinese =
      (langModule "Chinese" [ pkgs.noto-fonts ] {
        sansSerif = "Noto Sans CJK SC";
        serif = "Noto Serif CJK SC";
        monospace = "Noto Sans Mono";
      })
      // {
        configText = lib.mkOption {
          type = with lib.types; str;
          default = fontconfigs.english;
          readOnly = true;
          description = lib.mdDoc ''
            The fontconfig file of English fonts;
          '';
        };
      };

    emoji = {
      configText = lib.mkOption {
        type = with lib.types; str;
        default = fontconfigs.emoji;
        readOnly = true;
        description = lib.mdDoc ''
          The fontconfig file of emoji fonts;
        '';
      };

      defaultFont = lib.mkOption {
        type = with lib.types; str;
        default = "Noto Color Emoji";
        description = lib.mdDoc ''
          The default emoji font name.
        '';
      };

      defaultFonts = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ config.fonts.fontconfig.emoji.defaultFont ];
        description = lib.mdDoc ''
          Default emoji font(s).
        '';
      };

      enable = lib.mkOption {
        type = with lib.types; bool;
        default = false;
        description = lib.mdDoc ''
          Whether to enable emoji fontconfig.
        '';
      };

      fonts = lib.mkOption {
        type = with lib.types; listOf package;
        default = with pkgs; [ noto-fonts-color-emoji ];
        description = lib.mdDoc ''
          The fonts packages of emoji fontconfig will be used.
        '';
      };
    };

    english =
      (langModule "English" [ pkgs.noto-fonts ] {
        sansSerif = "Noto Sans";
        serif = "Noto Serif";
        monospace = "Noto Sans Mono";
      })
      // {
        configText = lib.mkOption {
          type = with lib.types; str;
          default = fontconfigs.english;
          readOnly = true;
          description = lib.mdDoc ''
            The fontconfig file of English fonts;
          '';
        };
      };

    symbol =
      let
        cfg = config.fonts.fontconfig.symbol;
      in
      {
        configText = lib.mkOption {
          type = with lib.types; str;
          default = fontconfigs.english;
          readOnly = true;
          description = lib.mdDoc ''
            The fontconfig file of English fonts;
          '';
        };

        defaultFont = {
          monospace = lib.mkOption {
            type = with lib.types; str;
            default = cfg.nerdFont.defaultFont.monospace;
            description = lib.mdDoc ''
              The default monospace symbol font name.
            '';
          };

          sansSerif = lib.mkOption {
            type = with lib.types; str;
            default = cfg.nerdFont.defaultFont.sansSerif;
            description = lib.mdDoc ''
              The default sans-serif symbol font name.
            '';
          };
        };

        defaultFonts = {
          monospace = lib.mkOption {
            type = with lib.types; listOf str;
            default = [ cfg.defaultFont.monospace ];
            description = lib.mdDoc ''
              Default monospace font(s).
            '';
          };

          sansSerif = lib.mkOption {
            type = with lib.types; listOf str;
            default = [ cfg.defaultFont.sansSerif ];
            description = lib.mdDoc ''
              Default sans serif font(s).
            '';
          };
        };

        enable = lib.mkOption {
          type = with lib.types; bool;
          default = false;
          description = lib.mdDoc ''
            Whether to enable emoji fontconfig.
          '';
        };

        fonts = lib.mkOption {
          type = with lib.types; listOf package;
          default = with pkgs; [ cfg.nerdFonts.package ];
          description = lib.mdDoc ''
            The fonts packages of symbol fontconfig will be used.
          '';
        };

        nerdFonts =
          let
            nerdFontPatches = [
              "0xProto"
              "3270"
              "Agave"
              "AnonymousPro"
              "Arimo"
              "AurulentSansMono"
              "BigBlueTerminal"
              "BitstreamVeraSansMono"
              "CascadiaCode"
              "CascadiaMono"
              "CodeNewRoman"
              "ComicShannsMono"
              "CommitMono"
              "Cousine"
              "D2Coding"
              "DaddyTimeMono"
              "DejaVuSansMono"
              "DroidSansMono"
              "EnvyCodeR"
              "FantasqueSansMono"
              "FiraCode"
              "FiraMono"
              "GeistMono"
              "Go-Mono"
              "Gohu"
              "Hack"
              "Hasklig"
              "HeavyData"
              "Hermit"
              "IBMPlexMono"
              "Inconsolata"
              "InconsolataGo"
              "InconsolataLGC"
              "IntelOneMono"
              "Iosevka"
              "IosevkaTerm"
              "IosevkaTermSlab"
              "JetBrainsMono"
              "Lekton"
              "LiberationMono"
              "Lilex"
              "MPlus"
              "MartianMono"
              "Meslo"
              "Monaspace"
              "Monofur"
              "Monoid"
              "Mononoki"
              "NerdFontsSymbolsOnly"
              "Noto"
              "OpenDyslexic"
              "Overpass"
              "ProFont"
              "ProggyClean"
              "Recursive"
              "RobotoMono"
              "ShareTechMono"
              "SourceCodePro"
              "SpaceMono"
              "Terminus"
              "Tinos"
              "Ubuntu"
              "UbuntuMono"
              "UbuntuSans"
              "VictorMono"
              "ZedMono"
              "iA-Writer"
            ];

            nerdFontPatch = lib.mkOptionType {
              inherit (lib.types.str) merge;

              name = "nerdFontPatch";
              description = "Nerd font patch";
              descriptionClass = "noun";
              check = x: elem x nerdFontPatches;
            };
          in
          {
            package = lib.mkOption {
              type = with lib.types; package;
              default = with pkgs; (nerdfonts.override { fonts = cfg.nerdFonts.includePatches; });
            };

            patches = lib.mkOption {
              type = with lib.types; listOf str;
              default = nerdFontPatches;
              readOnly = true;
              internal = true;
            };

            includePatches = lib.mkOption {
              type = with lib.types; listOf nerdFontPatch;
              description = lib.mdDoc ''
                Which Nerd Font will be included.

                Each value must be in the attrNames of https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/data/fonts/nerdfonts/shas.nix.
              '';
              default = [ "NerdFontsSymbolsOnly" ];
              example = "[\"SourceCodePro\"]";
            };

            defaultFont = {
              monospace = lib.mkOption {
                type = with lib.types; str;
                default = "Symbols Nerd Font Mono";
                description = lib.mdDoc ''
                  The default monospace nerd font name.
                '';
              };

              sansSerif = lib.mkOption {
                type = with lib.types; str;
                default = "Symbols Nerd Font";
                description = lib.mdDoc ''
                  The default sans-serif nerd font name.
                '';
              };
            };
          };
      };
  };

  config =
    let
      cfg = config.fonts.fontconfig;
    in
    with lib;
    mkIf cfg.enable (mkMerge [
      {
        fonts = {
          fontconfig = {
            confPackages = [ fcPkg ];
          };
        };
      }
      (mkIf cfg.chinese.enable {
        fonts = {
          packages = cfg.chinese.fonts;
        };
      })
      (mkIf cfg.emoji.enable {
        fonts = {
          packages = cfg.emoji.fonts;
        };
      })
      (mkIf cfg.english.enable {
        fonts = {
          packages = cfg.english.fonts;
        };
      })
      (mkIf cfg.symbol.enable {
        fonts = {
          packages = cfg.symbol.fonts;
        };
      })
    ]);
}
