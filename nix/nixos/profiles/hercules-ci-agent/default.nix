{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [ cachix ];
  };

  services = {
    hercules-ci-agent = {
      concurrentTasks = 4;
      enable = true;
    };
  };
}
