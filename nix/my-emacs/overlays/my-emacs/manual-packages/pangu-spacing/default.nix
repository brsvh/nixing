{
  emacs,
  fetchFromGitHub,
  lib,
  epkgs,
  ...
}:
let
  inherit (epkgs) trivialBuild;

  version = "0.4-2024-09-12";

  src = fetchFromGitHub {
    hash = "sha256-lu23V6GJDhM3ChMo+QGH81fh8JjZFTrf/4LkQxwdeac=";
    owner = "nailuoGG";
    repo = "pangu-spacing";
    rev = "000aa101c1d13db6bb6c04b8962d3c98a8e7b45c";
  };

  meta = {
    inherit (emacs.meta) platforms;

    description = "Emacs minor-mode to add space between CJK and English characters";
    homepage = "https://github.com/nailuoGG/pangu-spacing";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ brsvh ];
  };
in
trivialBuild {
  inherit meta src version;

  pname = "pangu-spacing";
}
