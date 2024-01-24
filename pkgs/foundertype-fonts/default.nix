{ fetchurl
, fonts ? [ ]
, lib
, stdenv
, ...
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

  knownFonts = attrNames shas;

  selectedFonts =
    if (fonts == [ ])
    then knownFonts
    else
      let
        unknown = subtractLists knownFonts fonts;
      in
      if (unknown != [ ])
      then throw "Unknown font(s): ${concatStringsSep " " unknown}"
      else
        fonts;

  selectedFontsShas =
    attrsets.genAttrs
      selectedFonts
      (
        name:
        shas."${name}"
      );

  srcs =
    attrsets.mapAttrsToList
      (
        name:
        sha:
        (
          fetchurl {
            url = "https://cdn1.foundertype.com/Public/Uploads/ttf/${name}.TTF";
            sha256 = sha;
          }
        )
      )
      selectedFontsShas;
in
mkDerivation {
  inherit srcs version;

  pname = "foundertype-fonts";

  sourceRoot = ".";

  buildPhase = ''
    echo "selected fonts are ${toString selectedFonts}"
  '';

  unpackPhase = ''
    :
  '';

  installPhase = ''
    mkdir -p $out/share/fonts/truetype/foundertype

    for src in $srcs; do
      cp $src $out/share/fonts/truetype/foundertype/
    done

  '';

  passthru = {
    updateScript = ./update.sh;
  };

  meta = {
    inherit license;
    description = "Simplify Chinese fonts distributed by FounderType";
    longDescription = ''
      FounderType Fonts is a unofficial collection of the fonts
      distributed by Founder Type.

      This package does not give you any rights to any of its included
      fonts, and only allows users who have obtained a license for
      FounderType to use them.
    '';
    homepage = "https://foundertype.com";
  };
}
