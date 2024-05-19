let
  name = "efi";
in
{
  inherit name;

  content = {
    format = "vfat";
    mountpoint = "/boot/efi";
    type = "filesystem";
  };

  label = name;
  priority = 1;
  size = "1G";
  type = "EF00";
}
