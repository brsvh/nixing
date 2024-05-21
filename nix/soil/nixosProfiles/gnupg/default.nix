{ pkgs, ... }:
{
  programs = {
    gnupg = {
      agent = {
        enable = true;
      };
    };
  };

  services = {
    dbus = {
      packages = with pkgs; [ gcr ];
    };
  };
}
