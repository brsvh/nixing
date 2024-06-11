{
  emacs,
  fetchFromGitHub,
  lib,
  epkgs,
  ...
}:
let
  inherit (epkgs) trivialBuild;

  version = "0-unstable-2024-06-11";

  src = fetchFromGitHub {
    hash = "sha256-4yMGBRRo3dFhOM9sMr10HnbJpWzsfbhuvhU6rOiY4P8=";
    owner = "ginqi7";
    repo = "sideline-eldoc";
    rev = "ffeecf0da1a007380fc2e6358ff90d3aad3db0db";
  };

  meta = {
    inherit (emacs.meta) platforms;

    description = "sideline backend for eldoc";
    homepage = "https://github.com/ginqi7/sideline-eldoc";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ brsvh ];
  };
in
trivialBuild rec {
  inherit meta src version;

  pname = "sideline-eldoc";

  buildInputs = propagatedUserEnvPkgs;

  propagatedUserEnvPkgs = with epkgs; [
    eldoc
    sideline
  ];
}
