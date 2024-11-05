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
        "com.tencent.wemeet" = {
          Context = {
            filesystems = [
              "home"
            ];
          };
        };
      };

      packages = [
        "com.tencent.wemeet"
      ];
    };
  };
}
