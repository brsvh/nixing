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
