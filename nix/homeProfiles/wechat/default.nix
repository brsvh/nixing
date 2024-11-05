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
        "com.tencent.WeChat" = {
          Context = {
            filesystems = [
              "home"
            ];
          };
        };
      };

      packages = [
        "com.tencent.WeChat"
      ];
    };
  };
}
