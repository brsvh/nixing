{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [ cachix ];
  };

  # TODO configure cachix agent.
}
