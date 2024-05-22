{ cell, ... }:
{
  imports = [ cell.homeProfiles.fonts ];

  fonts = {
    fontconfig = {
      japanese = {
        enable = true;
      };
    };
  };
}
