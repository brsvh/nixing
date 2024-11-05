{
  ...
}:
let
  inherit (builtins)
    head
    ;
in
{
  importSystem = systems: head (import systems);
}
