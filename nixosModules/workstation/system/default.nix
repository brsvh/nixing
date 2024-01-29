{ ... }:
{
  imports =
    [
      ./bootloader.nix
      ./console.nix
      ./i18n
      ./initrd.nix
      ./kernel.nix
      ./startup.nix
      ./swap.nix
    ];
}
