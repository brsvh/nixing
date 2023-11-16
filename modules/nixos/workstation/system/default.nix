{ ... }:
{
  imports =
    [
      ./bootloader.nix
      ./console.nix
      ./i18n.nix
      ./initrd.nix
      ./swap.nix
    ];
}
