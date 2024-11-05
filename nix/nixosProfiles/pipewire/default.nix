{
  lib,
  ...
}:
let
  inherit (lib)
    mkForce
    ;
in
{
  hardware = {
    pulseaudio = {
      enable = mkForce false;
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

      wireplumber = {
        enable = true;
      };
    };
  };
}
