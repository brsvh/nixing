{ ... }:
{
  imports =
    [
      ./bootloader.nix
      ./console.nix
      ./i18n.nix
      ./initrd.nix
      ./kernel.nix
      ./startup.nix
      ./swap.nix
    ];
}
