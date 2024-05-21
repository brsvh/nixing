{ lib, ... }:
{
  hardware = {
    pulseaudio = {
      enable = lib.mkForce false;
    };
  };

  services = {
    pipewire = {
      enable = true;

      alsa = {
        enable = true;
        support32Bit = true;
      };

      jack = {
        enable = true;
      };

      pulse = {
        enable = true;
      };
    };
  };
}
