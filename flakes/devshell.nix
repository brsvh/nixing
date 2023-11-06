{ inputs
, ...
}:
{
  imports = [
    inputs.devshell.flakeModule
  ];

  perSystem =
    { config
    , inputs'
    , pkgs
    , ...
    }: {
      devshells = {
        default = {
          name = "nixing:default";
          commands = [
            {
              package = pkgs.age;
              category = "secrets";
            }
            {
              package = pkgs.git;
              category = "development";
            }
            {
              package = pkgs.nixUnstable;
              category = "development";
            }
            {
              package = pkgs.joe;
              category = "development";
            }
            {
              package = pkgs.sops;
              category = "secrets";
            }
            {
              package = pkgs.ssh-to-age;
              category = "secrets";
            }
          ];
          env = [
            {
              name = "EDITOR";
              value = "jmacs";
            }
          ];
        };
      };
    };
}
