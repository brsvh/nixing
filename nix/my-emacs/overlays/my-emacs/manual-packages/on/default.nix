{
  fetchFromGitLab,
  lib,
  emacs,
  epkgs,
}:
let
  inherit (epkgs) trivialBuild;

  version = "0.1.0-2024-05-28";

  src = fetchFromGitLab {
    owner = "ajgrf";
    repo = "on.el";
    rev = "3cf623e1a4331e259ef92e49154ed0551f300436";
    hash = "sha256-gtSVCpQwv4Ui9VpW7SXnsXIkfHN/6laMLqHTezDcMZg=";
  };

  meta = with lib; {
    inherit (emacs.meta) platforms;

    description = "utility hooks and functions from Doom Emacs";
    homepage = "https://gitlab.com/ajgrf/on.el";
    license = licenses.mit;
    maintainers = with maintainers; [ brsvh ];
  };
in
trivialBuild rec {
  inherit meta src version;

  pname = "on";
}
