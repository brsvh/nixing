{
  fetchurl,
  lib,
  stdenv,
  symlinkJoin,
  ...
}:
with builtins;
with lib;
let
  inherit (stdenv) mkDerivation;

  license = {
    shortName = "foundertype-he-ula";
    fullName = "方正字库家庭版用户许可协议";
    url = "https://www.foundertype.com/index.php/About/powerPer.html";
    free = false;
    redistributable = false;
  };

  shas = import ./shas.nix;

  version = import ./version.nix;

  mkFontDerivation =
    font: sha256:
    mkDerivation {
      inherit version;

      pname = "foundertype-${font}";

      src = fetchurl {
        inherit sha256;
        url = "https://cdn1.foundertype.com/Public/Uploads/ttf/${font}.TTF";
      };

      unpackPhase = ''
        :
      '';

      installPhase = ''
        runHook preInstall        

        install -Dm444 $src $out/share/fonts/truetype/foundertype/${font}.ttf;

        runHook postInstall
      '';

      meta = {
        inherit license;

        description = "${font} font distributed by FounderType";

        longDescription = ''
          ${font} font a unfree font distributed by Founder Type.

          This package does not give you any rights to any of its included
          fonts, and only allows users who have obtained a license for
          FounderType to use them.
        '';

        homepage = "https://foundertype.com";
      };
    };

  fontDerivations = mapAttrsToList (font: sha: (mkFontDerivation font sha)) shas;
in
symlinkJoin rec {
  inherit version;
  pname = "foundertype-fonts";
  name = "${pname}-${version}";
  paths = fontDerivations;
}
