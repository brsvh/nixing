{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins)
    attrNames
    filter
    head
    length
    listToAttrs
    typeOf
    ;

  inherit (lib)
    attrByPath
    forEach
    mdDoc
    mkOption
    types
    ;

  languages = [
    "chinese"
  ];

  ibusEngine = types.mkOptionType {
    inherit (types.package)
      descriptionClass
      merge
      ;

    name = "ibus-engine";
    check =
      x:
      (types.package.check x)
      && (attrByPath [
        "meta"
        "isIbusEngine"
      ] false x);
  };

  mkLangOptions = lang: {
    enable = mkOption {
      default = false;

      description = mdDoc ''
        Whether to enable ${lang} support.
      '';

      type = types.bool;
    };

    engine = mkOption {
      default = null;

      description = mdDoc ''
        The engine of ibus for "${lang}" will use.
      '';

      type = types.nullOr ibusEngine;
    };

    engineName = mkOption {
      default =
        let
          engines = pkgs.ibus-engines;

          engines' = attrNames engines;

          engine = config.i18n.inputMethod.ibus.${lang}.engine;

          matched = filter (n: engines.${n} == engine) engines';
        in
        if length matched > 0 then head matched else null;

      description = mdDoc ''
        The name of the ibus engine of "${lang}"
      '';

      internal = true;
      readOnly = true;
      type = with types; nullOr str;
    };
  };
in
{
  options = {
    i18n = {
      inputMethod = {
        ibus = listToAttrs (
          forEach languages (lang: {
            name = lang;
            value = mkLangOptions lang;
          })
        );
      };
    };
  };

  config =
    let
      cfg = config.i18n.inputMethod.ibus;

      engines =
        let
          engines' = map (lang: cfg.${lang}.engine) languages;
        in
        filter (p: (typeOf p) != "null") engines';
    in
    {
      i18n = {
        inputMethod = {
          ibus = {
            inherit engines;
          };
        };
      };
    };
}
