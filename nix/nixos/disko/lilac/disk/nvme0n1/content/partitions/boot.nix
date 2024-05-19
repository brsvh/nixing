let
  name = "boot";
in
{
  inherit name;

  content = {
    format = "ext4";
    mountpoint = "/boot";
    type = "filesystem";
  };

  label = name;
  priority = 2;
  size = "1G";
  type = "EF02";
}
