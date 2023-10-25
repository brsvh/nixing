{ inputs
, ...
}: {
  imports =
    [
      inputs.devshell.flakeModule
      inputs.pre-commit.flakeModule
    ];

  perSystem =
    { config
    , ...
    }: {
      pre-commit = {
        check = {
          enable = true;
        };
        settings = {
          hooks = {
            nixpkgs-fmt = {
              enable = true;
            };
          };
        };
      };

      devshells = {
        default = {
          devshell = {
            startup = {
              pre-commit-hook = {
                text = config.pre-commit.installationScript;
              };
            };
          };
        };
      };
    };
}
