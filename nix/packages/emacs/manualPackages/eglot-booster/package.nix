{
  eglot,
  fetchFromGitHub,
  jsonrpc,
  lib,
  seq,
  trivialBuild,
  ...
}:
let
  inherit (lib)
    licenses
    maintainers
    ;

  version = "0.0.1";

  src = fetchFromGitHub {
    hash = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs=";
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
  };

  meta = {
    description = "Boost eglot using lsp-booster";
    homepage = "https://github.com/jdtsmith/eglot-booster";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ brsvh ];
  };
in
trivialBuild rec {
  inherit
    meta
    src
    version
    ;

  pname = "eglot-booster";

  buildInputs = propagatedUserEnvPkgs;

  propagatedUserEnvPkgs = [
    eglot
    jsonrpc
    seq
  ];
}
