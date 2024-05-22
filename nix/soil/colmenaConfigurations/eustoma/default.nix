{ cell, ... }:
{
  imports = [ cell.nixosConfigurations.eustoma ];

  deployment = {
    allowLocalDeployment = true;

    tags = [
      "all"
      "eustoma"
    ];
  };
}
