{
  pkgs,
  ...
}:
{
  environment = {
    systemPackages = with pkgs; [
      cachix
    ];
  };

  services = {
    hercules-ci-agent = {
      enable = true;

      settings = {
        concurrentTasks = 4;
      };
    };
  };
}
