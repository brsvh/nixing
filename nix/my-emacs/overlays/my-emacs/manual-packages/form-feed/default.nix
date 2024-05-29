{
  fetchgit,
  lib,
  emacs,
  epkgs,
}:
let
  inherit (epkgs) trivialBuild;

  version = "0.2.3-2024-05-28";

  src = fetchgit {
    url = "https://depp.brause.cc/form-feed.git";
    rev = "ac1f0ef30a11979f5dfe12d8c05a666739e486ff";
    hash = "sha256-bwkVS2+f0OeUh8m7QkdcDzCHiHU29rBDAhMvjg2zOuc=";
  };

  meta = with lib; {
    inherit (emacs.meta) platforms;

    description = "display ^L glyphs as horizontal lines";
    homepage = "https://depp.brause.cc/form-feed/form-feed.el";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ brsvh ];
  };
in
trivialBuild rec {
  inherit meta src version;

  pname = "form-feed";
}
