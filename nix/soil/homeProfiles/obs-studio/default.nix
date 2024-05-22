{ pkgs, ... }:
{
  programs = {
    obs-studio = {
      enable = true;

      plugins = with pkgs; [ obs-pipewire-audio-capture ];
    };
  };
}
