{ mkTsangerTypeFontDerivation, ... }:
mkTsangerTypeFontDerivation {
  fontPinyinName = import ./pinyin.nix;
  gratisProCommercium = import ./gratis-pro-commercium.nix;
  product = import ./product.nix;
  sha256 = import ./sha256.nix;
  updateScript = ./update.sh;
  variant = import ./variant.nix;
  version = import ./version.nix;
  weight = import ./weight.nix;
}
