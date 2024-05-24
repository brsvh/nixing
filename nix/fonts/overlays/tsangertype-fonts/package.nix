{
  callPackage,
  fetchurl,
  generateSplicesForMkScope,
  lib,
  makeScopeWithSplicing',
  pkgs,
  stdenvNoCC,
  xorg,
  ...
}:
let
  mkTsangerTypeFontDerivation =
    {
      baseURL ? "http://tsanger.cn/download/",
      fontPinyinName, # str
      gratisProCommercium ? false,
      product, # str
      sha256, # str
      variant ? null, # str
      updateScript,
      version, # str
      weight ? null, # str
    }:
    let
      canonicalName =
        fontPinyinName
        + (if variant != null then "-" + variant else "")
        + (if weight != null then "-w" + weight else "");

      canonicalName' = "tsangertype-" + canonicalName;

      licenses = {
        # Gratis propria commercium.
        gpc = {
          free = false;
          fullName = "仓耳字库免费商用字体授权";
          redistributable = true;
          shortName = "tsangertype-gpc-license";
          url = "http://www.tsanger.cn/仓耳字库免费商用字体授权声明.pdf";
        };

        # Gratis propria persona.
        gpp = {
          free = false;
          fullName = "仓耳字库个人非商用字体授权";
          # REVIEW check redistributability.
          redistributable = true;
          shortName = "tsangertype-gpp-license";
          url = "http://www.tsanger.cn/TODO";
        };
      };

      meta = {
        license = if gratisProCommercium then licenses.gpc else licenses.gpp;
        description = "${fontPinyinName} font family distributed by tsangertype";
        longDescription = ''
          ${fontPinyinName} font${if variant != null then ", variant " + variant else ""}${
            if variant != null then ", weight " + weight else ""
          }.

          This font is ${
            if gratisProCommercium then "gratis propria commercium" else "only gratis propria persona"
          }, origin source is distribute by TsangerType Inc.
        '';
        maintainers = with lib.maintainers; [ brsvh ];
      };

      pname = "${canonicalName'}-font";

      src = fetchurl {
        inherit sha256;

        name = canonicalName' + ".ttf";
        url = "${baseURL + product}.ttf";
      };
    in
    stdenvNoCC.mkDerivation {
      inherit
        meta
        pname
        src
        version
        ;

      unpackPhase = ''
        :
      '';

      installPhase = ''
        runHook preInstall

        install -Dm444 $src $out/share/fonts/truetype/tsangertype/${canonicalName}.ttf;

        runHook postInstall
      '';

      passthru = {
        inherit updateScript;
      };
    };

  callPackage' = pkg: attrs: callPackage pkg (attrs // { inherit mkTsangerTypeFontDerivation; });

  tsangertypeFonts = lib.packagesFromDirectoryRecursive {
    callPackage = callPackage';
    directory = ./fonts;
  };

  collect =
    self:
    let
      linkTsangerTypeFonts =
        name: drvs:
        stdenvNoCC.mkDerivation {
          inherit name;

          buildInputs = [ xorg.lndir ] ++ drvs;

          unpackPhase = "true";

          installPhase = ''
            runHook preInstall

            mkdir -p $out
            for drv in ${lib.concatStringsSep " " drvs}; do
              ${xorg.lndir}/bin/lndir -silent $drv $out
            done

            runHook postInstall
          '';
        };

      listOfFontsWithCond =
        # cond must be a conditional fucntion can check derivation.
        drvs: cond: with lib; attrValues (filterAttrs (n: v: isDerivation v && (cond v)) drvs);

      licensePredicate =
        drv: lic:
        (lib.attrByPath [
          "meta"
          "license"
          "shortName"
        ] "unknown" drv) == lic;

      listOfGratisProCommerciumFonts =
        drvs:
        let
          cond = drv: licensePredicate drv "tsangertype-gpc-license";
        in
        listOfFontsWithCond drvs cond;

      listOfGratisProPersonaFonts =
        drvs:
        with lib;
        let
          cond = drv: licensePredicate drv "tsangertype-gpp-license";
        in
        listOfFontsWithCond drvs cond;

      combine' =
        fontList:
        {
          name ? "tsangertype-combined-fonts",
          ...
        }@args:
        linkTsangerTypeFonts name fontList;

      combine = fontList: combine' fontList { };

      gratisProCommercium =
        let
          comm = (listOfGratisProCommerciumFonts tsangertypeFonts);
        in
        combine' comm { name = "tsangertype-gpc-fonts"; };

      # All fonts.
      gratisProPersona =
        let
          comm = (listOfGratisProCommerciumFonts tsangertypeFonts);

          per = (listOfGratisProPersonaFonts tsangertypeFonts) ++ comm;
        in
        combine' per { name = "tsangertype-fonts"; };
    in
    tsangertypeFonts
    // {
      inherit
        combine
        combine'
        gratisProCommercium
        gratisProPersona
        listOfFontsWithCond
        listOfGratisProCommerciumFonts
        listOfGratisProPersonaFonts
        ;
    };
in
makeScopeWithSplicing' {
  otherSplices = generateSplicesForMkScope "tsangertypeFonts";
  f = collect;
}
