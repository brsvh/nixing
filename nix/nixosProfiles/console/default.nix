{
  lib,
  ...
}:
let
  inherit (lib)
    mkDefault
    ;
in
{
  console = {
    earlySetup = true;
    font = mkDefault "eurlatgr";
  };
}
