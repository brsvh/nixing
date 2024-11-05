{
  config,
  ...
}:
{
  boot = {
    extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];
  };

  hardware = {
    nvidia = {
      open = true;
    };
  };
}
