{
  fetchgit,
  lib,
  trivialBuild,
  ...
}:
let
  inherit (lib)
    licenses
    maintainers
    ;

  version = "0.2.3";

  src = fetchgit {
    url = "https://depp.brause.cc/form-feed.git";
    rev = "ac1f0ef30a11979f5dfe12d8c05a666739e486ff";
    hash = "sha256-bwkVS2+f0OeUh8m7QkdcDzCHiHU29rBDAhMvjg2zOuc=";
  };

  meta = {
    description = "display ^L glyphs as horizontal lines";
    homepage = "https://depp.brause.cc/form-feed/form-feed.el";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ brsvh ];
  };
in
trivialBuild {
  inherit
    meta
    src
    version
    ;

  pname = "form-feed";
}
