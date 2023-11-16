{ ... }:
{
  imports =
    [
      ./bootloader.nix
      ./console.nix
      ./i18n.nix
      ./initrd.nix
      ./startup.nix
      ./swap.nix
    ];
}
