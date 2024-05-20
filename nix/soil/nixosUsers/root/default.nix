{ cell, pkgs, ... }:
{
  imports = [ cell.nixosProfiles.fish ];

  users = {
    users = {
      root = {
        initialHashedPassword = "$6$BnGvScsfnCRh3coN$yo.YsllSelnixuiWuiMFQYaJrneNqLrfUAhEOMw6CN/Od2kZSLdclJaa4h1TBtBP7NeWxBKsIsftFZQB46DUV.";
        shell = pkgs.fish;
      };
    };
  };
}
