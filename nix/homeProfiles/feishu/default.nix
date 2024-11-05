{
  my,
  ...
}:
{
  imports = [
    my.homeProfiles.flatpak
  ];

  services = {
    flatpak = {
      overrides = {
        "cn.feishu.Feishu" = {
          Context = {
            filesystems = [
              "home"
            ];
          };
        };
      };

      packages = [
        "cn.feishu.Feishu"
      ];
    };
  };
}
