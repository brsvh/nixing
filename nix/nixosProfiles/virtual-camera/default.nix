{
  config,
  ...
}:
{
  boot = {
    extraModprobeConfig = ''
      options v4l2loopback devices=1 video_nr=1 card_label="Virtual Camera" exclusive_caps=1
    '';

    extraModulePackages = with config.boot.kernelPackages; [
      v4l2loopback
    ];

    kernelModules = [
      "v4l2loopback"
    ];
  };
}
