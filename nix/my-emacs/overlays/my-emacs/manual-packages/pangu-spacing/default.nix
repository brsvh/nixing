{
  emacs,
  fetchFromGitHub,
  lib,
  epkgs,
  ...
}:
let
  inherit (epkgs) trivialBuild;

  version = "0.4-2024-09-14";

  src = fetchFromGitHub {
    hash = "sha256-kPEoM/cWKF1/KX5sQw1YIpKAab+QIJnUGdMR+Mw2KTQ=";
    owner = "brsvh";
    repo = "pangu-spacing";
    rev = "4dd9f7a37ffa75a4083d20163d93c25a86952754";
  };

  meta = {
    inherit (emacs.meta) platforms;

    description = "Emacs minor-mode to add space between CJK and English characters";
    homepage = "https://github.com/brsvh/pangu-spacing";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ brsvh ];
  };
in
trivialBuild {
  inherit meta src version;

  pname = "pangu-spacing";
}
