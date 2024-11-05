{
  fetchFromGitHub,
  lib,
  trivialBuild,
  ...
}:
let
  inherit (lib)
    licenses
    maintainers
    ;

  version = "0.4";

  src = fetchFromGitHub {
    hash = "sha256-kPEoM/cWKF1/KX5sQw1YIpKAab+QIJnUGdMR+Mw2KTQ=";
    owner = "brsvh";
    repo = "pangu-spacing";
    rev = "4dd9f7a37ffa75a4083d20163d93c25a86952754";
  };

  meta = {
    description = "Emacs minor-mode to add space between CJK and English characters";
    homepage = "https://github.com/brsvh/pangu-spacing";
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

  pname = "pangu-spacing";
}
