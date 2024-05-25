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
          The fonts packages of ${lang} will be used.
        '';
      };

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
        inherit (config.fonts.fontconfig.chinese) sansSerif serif monospace;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>zh</string>
          </test>
          <test name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${serif}</string>
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
            <string>${sansSerif}</string>
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
            <string>${monospace}</string>
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
            <string>${sansSerif}</string>
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
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>WenQuanYi Zen Hei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>WenQuanYi Micro Hei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>WenQuanYi Micro Hei Light</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>Microsoft YaHei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>SimHei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>SimSun</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>SimSun-18030</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${serif}</string>
          </edit>
        </match>
      '';

    emoji =
      let
        inherit (config.fonts.fontconfig.emoji) defaultFont;
      in
      ''
        <match>
          <test qual="any" name="family">
            <string>emoji</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${defaultFont}</string>
          </edit>
        </match>
      '';

    english =
      let
        inherit (config.fonts.fontconfig.english) sansSerif serif monospace;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>en</string>
          </test>
          <test qual="any" name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${serif}</string>
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
            <string>${sansSerif}</string>
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
            <string>${monospace}</string>
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
            <string>${sansSerif}</string>
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
            <string>${sansSerif}</string>
          </edit>
        </match>
      '';

    japanese =
      let
        inherit (config.fonts.fontconfig.japanese) sansSerif serif monospace;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ja</string>
          </test>
          <test name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ja</string>
          </test>
          <test name="family">
            <string>sans-serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ja</string>
          </test>
          <test name="family">
            <string>monospace</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${monospace}</string>
          </edit>
        </match>
      '';

    korean =
      let
        inherit (config.fonts.fontconfig.korean) sansSerif serif monospace;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ko</string>
          </test>
          <test name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ko</string>
          </test>
          <test name="family">
            <string>sans-serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ko</string>
          </test>
          <test name="family">
            <string>monospace</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${monospace}</string>
          </edit>
        </match>
      '';
  };

  mkFC =
    name: config:
    let
      pname = "fc-51-local-" + name + ".conf";
    in
    pkgs.writeText pname config;

  fcConf =
    let
      cfg = config.fonts.fontconfig;

      getFC = type: if cfg."${type}".enable then fontconfigs."${type}" else "";

      getDF = type: if cfg."${type}".enable then fontconfigs."${type}" else null;
    in
    mkFC "local-fontconfig" ''
      <?xml version='1.0'?>
      <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
      <fontconfig>
        <description>Local fontconfig</description>

        ${getFC "emoji"}

        ${getFC "english"}

        ${getFC "chinese"}

        ${getFC "japanese"}

        ${getFC "korean"}

      </fontconfig>
    '';
in
{
  options.fonts.fontconfig = {
    chinese =
      langModule "Chinese"
        (with pkgs; [
          noto-fonts-cjk-sans
          noto-fonts-cjk-serif
        ])
        {
          sansSerif = "Noto Sans CJK SC";
          serif = "Noto Serif CJK SC";
          monospace = "Noto Sans Mono CJK SC";
        };

    emoji = {
      defaultFont = lib.mkOption {
        type = with lib.types; str;
        default = "Noto Color Emoji";
        description = lib.mdDoc ''
          The default emoji font name.
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

    english = langModule "English" [ pkgs.noto-fonts ] {
      sansSerif = "Noto Sans";
      serif = "Noto Serif";
      monospace = "Noto Sans Mono";
    };

    japanese =
      langModule "Japanese"
        (with pkgs; [
          noto-fonts-cjk-sans
          noto-fonts-cjk-serif
        ])
        {
          sansSerif = "Noto Sans CJK JP";
          serif = "Noto Serif CJK JP";
          monospace = "Noto Sans Mono CJK JP";
        };

    korean =
      langModule "Korean"
        (with pkgs; [
          noto-fonts-cjk-sans
          noto-fonts-cjk-serif
        ])
        {
          sansSerif = "Noto Sans CJK KR";
          serif = "Noto Serif CJK KR";
          monospace = "Noto Sans Mono CJK KR";
        };

    symbol =
      let
        cfg = config.fonts.fontconfig.symbol;
      in
      {
        defaultFont = lib.mkOption {
          type = with lib.types; str;
          default = cfg.nerdFonts.defaultFont;
          description = lib.mdDoc ''
            The default symobol font name.
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
          default = with pkgs; [ cfg.nerdFonts.package ];
          description = lib.mdDoc ''
            The fonts packages of symbol will be used.
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
            defaultFont = lib.mkOption {
              type = with lib.types; str;
              default = "Symbols Nerd Font Mono";
              description = lib.mdDoc ''
                The default Nerd Font name.
              '';
            };

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
            defaultFonts = {
              sansSerif =
                let
                  getLang = type: optional cfg."${type}".enable cfg."${type}".sansSerif;
                in
                (getLang "english")
                ++ (getLang "chinese")
                ++ (getLang "japanese")
                ++ (getLang "korean")
                ++ (optional cfg.emoji.enable cfg.emoji.defaultFont)
                ++ (optional cfg.symbol.enable cfg.symbol.defaultFont);

              serif =
                let
                  getLang = type: optional cfg."${type}".enable cfg."${type}".serif;
                in
                (getLang "english")
                ++ (getLang "chinese")
                ++ (getLang "japanese")
                ++ (getLang "korean")
                ++ (optional cfg.emoji.enable cfg.emoji.defaultFont)
                ++ (optional cfg.symbol.enable cfg.symbol.defaultFont);

              monospace =
                let
                  getLang = type: optional cfg."${type}".enable cfg."${type}".monospace;
                in
                (getLang "english")
                ++ (getLang "chinese")
                ++ (getLang "japanese")
                ++ (getLang "korean")
                ++ (optional cfg.emoji.enable cfg.emoji.defaultFont)
                ++ (optional cfg.symbol.enable cfg.symbol.defaultFont);
            };
          };
        };

        xdg = {
          configFile = {
            "fontconfig/conf.d/51-local-fontconfig.conf".source = fcConf;
          };

          enable = true;
        };
      }
      (mkIf cfg.chinese.enable {
        home = {
          packages = cfg.chinese.fonts;
        };
      })
      (mkIf cfg.emoji.enable {
        home = {
          packages = cfg.emoji.fonts;
        };
      })
      (mkIf cfg.english.enable {
        home = {
          packages = cfg.english.fonts;
        };
      })
      (mkIf cfg.japanese.enable {
        home = {
          packages = cfg.japanese.fonts;
        };
      })
      (mkIf cfg.korean.enable {
        home = {
          packages = cfg.korean.fonts;
        };
      })
      (mkIf cfg.symbol.enable {
        home = {
          packages = cfg.symbol.fonts;
        };
      })
    ]);
}
