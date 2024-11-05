{
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    filter
    ;

  inherit (pkgs.stdenv)
    system
    ;

  allSystems = [
    "aarch64-linux"
    "aarch64_be-linux"
    "alpha-linux"
    "armv6l-linux"
    "armv7l-linux"
    "i386-linux"
    "i486-linux"
    "i586-linux"
    "i686-linux"
    "i686-windows"
    "loongarch64-linux"
    "mips-linux"
    "mips64-linux"
    "mips64-linuxabin32"
    "mips64el-linux"
    "mips64el-linuxabin32"
    "mipsel-linux"
    "powerpc-linux"
    "powerpc64-linux"
    "powerpc64le-linux"
    "riscv32-linux"
    "riscv64-linux"
    "s390x-linux"
    "sparc-linux"
    "sparc64-linux"
    "wasm32-wasi"
    "wasm64-wasi"
    "x86_64-linux"
    "x86_64-windows"
  ];

  excludeCurrentSystem = s: filter (s': s' != s) allSystems;
in
{
  boot = {
    binfmt = {
      emulatedSystems = excludeCurrentSystem system;
    };
  };
}
