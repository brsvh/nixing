{ cell, ... }:
{
  imports = [ cell.nixosConfigurations.lilac ];

  deployment = {
    allowLocalDeployment = true;

    tags = [
      "all"
      "lilac"
    ];
  };
}
