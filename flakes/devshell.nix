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
              package = inputs'.brsvh-emacs.packages.nogui;
              help = "The extensible, customizable GNU text editor";
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
              value = "emacs";
            }
          ];
        };
      };
    };
}
