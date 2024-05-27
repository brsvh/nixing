{
  emacs,
  fetchFromGitHub,
  lib,
  epkgs,
  ...
}:
let
  inherit (epkgs) trivialBuild;

  version = "0.0.1-2024-05-28";

  src = fetchFromGitHub {
    hash = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs=";
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
  };

  meta = {
    inherit (emacs.meta) platforms;

    description = "Boost eglot using lsp-booster";
    homepage = "https://github.com/jdtsmith/eglot-booster";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ brsvh ];
  };
in
trivialBuild rec {
  inherit meta src version;

  pname = "eglot-booster";

  buildInputs = propagatedUserEnvPkgs;

  propagatedUserEnvPkgs = with epkgs; [
    eglot
    jsonrpc
    seq
  ];
}
