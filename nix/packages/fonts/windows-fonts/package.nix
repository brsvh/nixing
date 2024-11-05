{
  fetchurl,
  lib,
  p7zip,
  stdenvNoCC,
  ...
}:
let
  inherit (lib)
    maintainers
    ;

  version = "26100.1742.240906-0331";

  license = {
    shortName = "microsoft-software-license";
    fullName = "Microsoft Software License Terms";
    url = "https://support.microsoft.com/en-us/windows/microsoft-software-license-terms-e26eedad-97a2-5250-2670-aad156b654bd";
    free = false;
    redistributable = false;
  };

  src = fetchurl {
    hash = "sha256-dVqQ1D6CanS54ZMqNHiLiY4CgnJDm3d+VZPe6NU2Iq4=";
    url = "https://software-static.download.prss.microsoft.com/dbazure/888969d5-f34g-4e03-ac9d-1f9786c66749/26100.1742.240906-0331.ge_release_svc_refresh_CLIENTENTERPRISEEVAL_OEMRET_x64FRE_en-us.iso";
  };

  meta = {
    inherit
      license
      ;

    description = "Windows fonts distributed by Microsoft Co.";
    homepage = "https://learn.microsoft.com/en-us/typography/fonts/font-faq";

    longDescription = ''
      Windows fonts are proprietary software distributed by Microsoft Co.

      This package does not give you any rights to any of its included
      fonts.
    '';

    maintainers = with maintainers; [ brsvh ];
    redistributable = false;
  };
in
stdenvNoCC.mkDerivation {
  inherit
    meta
    src
    version
    ;

  pname = "windows-fonts";

  buildInputs = [
    p7zip
  ];

  nativeBuildInputs = [
    p7zip
  ];

  unpackPhase = ''
    runHook preUnpack

    tempdir=$(mktemp -d)

    7z x $src -o$tempdir

    mkdir fonts

    7z e $tempdir/sources/install.wim Windows/{Fonts/"*".{ttf,ttc},System32/Licenses/neutral/"*"/"*"/license.rtf} -ofonts

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype/Microsoft/

    cp fonts/* $out/share/fonts/truetype/Microsoft/

    runHook postInstall
  '';
}
