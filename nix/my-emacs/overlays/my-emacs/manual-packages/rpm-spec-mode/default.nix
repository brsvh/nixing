{
  emacs,
  fetchFromGitHub,
  lib,
  epkgs,
  ...
}:
let
  inherit (epkgs) trivialBuild;

  version = "0-unstable-2024-06-20";

  src = fetchFromGitHub {
    hash = "sha256-L1Aoxqze5OPAs2TA2PD31AhvilYB0MLiMTT675c+ahI=";
    owner = "bhavin192";
    repo = "rpm-spec-mode";
    rev = "e95001cf4e85d9d67a43a5c5f8f088fbf4492c15";
  };

  meta = {
    inherit (emacs.meta) platforms;

    description = "RPM spec file editing commands for Emacs/XEmacs";
    homepage = "https://github.com/bhavin192/rpm-spec-mode/";
    license = lib.licenses.gpl2Plus;
    maintainers = with lib.maintainers; [ brsvh ];
  };
in
trivialBuild rec {
  inherit meta src version;

  pname = "rpm-spec-mode";
}
