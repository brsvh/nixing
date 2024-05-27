with builtins;
let
  recipe = (fromJSON (readFile ./flake.lock)).nodes.flake-compat;

  source = fetchTree {
    inherit (recipe.locked)
      repo
      rev
      type
      owner
      ;
  };
in
(import source { src = ./.; }).defaultNix
