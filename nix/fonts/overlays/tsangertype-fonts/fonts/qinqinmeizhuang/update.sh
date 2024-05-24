#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch python3

printf '"0-unstable-%s"\n' "$(date "+%Y-%m-%d")" > version.nix

dirname="$(dirname "$0")"

baseurl="http://tsanger.cn/download/"
name="$(sed 's/"//g' "$dirname/product.nix")"
pinyin_name="$(sed 's/"//g' "$dirname/pinyin.nix")"
variant="$(sed 's/"//g' "$dirname/variant.nix")"
weight="$(sed 's/"//g' "$dirname/weight.nix")"
ext="ttf"
url="${baseurl}${name}.${ext}"

canonical_name="$pinyin_name"

if [ "$variant" != "null" ]; then
  canonical_name="${canonical_name}-${variant}"
fi

if [ "$weight" != "null" ]; then
  canonical_name="${canonical_name}-w${weight}"
fi

canonical_name="${canonical_name}.ttf"

printf '"%s"\n' "$(nix-prefetch-url "$url" --name "$canonical_name" --type sha256)" > "$dirname/sha256.nix"
