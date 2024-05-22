#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch

echo "\"0-unstable-$(date "+%Y-%m-%d")\"" > version.nix

dirname="$(dirname "$0")"

fonts="$(cat "$dirname"/urls.json)"

printf '{\n' > "$dirname/shas.nix"

while
  read -r name
  read -r url
do
    printf '  "%s" = "%s";\n' "${name%%.*}" "$(nix-prefetch-url "$url")" >>"$dirname/shas.nix"
done < <(jq -r '.fonts[] | select(.name | test("TTF")) | .name, .url' <<<"$fonts")

printf '}\n' >> "$dirname/shas.nix"
