{
  lib,
  makeWrapper,
  runCommandLocal,
  stdenv,
  xorg,
}:
with lib;
{
  binaries,
  fonts,
  initDirectory,
  libraries,
  plainEmacs,
  vanillaEmacs,
}:
stdenv.mkDerivation {
  inherit (vanillaEmacs) meta;

  name = "my-" + vanillaEmacs.name;

  preferLocalBuild = true;

  buildInputs = [
    initDirectory
    plainEmacs
    xorg.lndir
    makeWrapper
  ];

  runtimeInputs = [ ];

  phases = [ "installPhase" ];

  installPhase = ''
    runHook preInstall

    mkdir $out
    ${xorg.lndir}/bin/lndir -silent ${plainEmacs} $out

    mv $out/bin/emacs $out/bin/emacs-unwrapped

    makeWrapper $out/bin/emacs-unwrapped $out/bin/emacs \
      --prefix PATH : ${makeBinPath binaries} \
      --prefix LD_LIBRARY_PATH : ${makeLibraryPath libraries} \
      --add-flags "--init-directory=${initDirectory}"

    runHook postInstall
  '';
}
