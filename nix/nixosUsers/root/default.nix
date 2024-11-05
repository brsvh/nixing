{
  config,
  my,
  ...
}:
{
  imports = [
    my.nixosProfiles.fish
  ];

  users = {
    users = {
      root = {
        initialHashedPassword = "$6$OKxyX8LiVd/RmeVj$CwpXDNgDjJ0FtGg71xxy88R8lBnN/IWk.wzlIQA9gvp56beeLT1asQhKsboaA2SB1xUfcdxSqtwB9eZ/NPeoj.";
        shell = config.programs.fish.package;
      };
    };
  };
}
