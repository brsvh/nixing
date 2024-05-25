# MIT License

# Copyright (c) 2020-2021 Lin Yinfeng
# Copyright (c) 2024 Burgess Chang

# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
{
  autoPatchelfHook,
  callPackage,
  dpkg,
  fd,
  fetchurl,
  lib,
  libjpeg8,
  libyuv,
  makeDesktopItem,
  qt5,
  rsync,
  system,
}:

let
  sourceInfo = builtins.fromJSON (lib.readFile ./source.json);

  desktopItem = makeDesktopItem {
    name = "wemeetapp";
    desktopName = "Wemeet";
    exec = "wemeetapp %u";
    icon = "wemeetapp";

    categories = [ "AudioVideo" ];

    mimeTypes = [ "x-scheme-handler/wemeet" ];

    extraConfig = {
      "Name[zh_CN]" = "腾讯会议";
    };
  };

  desktopItemForceX11 = desktopItem.override {
    name = "wemeetapp-force-x11";
    desktopName = "Wemeet (X)";
    exec = "wemeetapp-force-x11 %u";

    extraConfig = {
      "Name[zh_CN]" = "腾讯会议 (X)";
    };
  };
in
qt5.mkDerivation {
  inherit (sourceInfo.${system}) version;

  pname = "wemeet";

  src = fetchurl { inherit (sourceInfo.${system}) url sha512; };

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    fd
    rsync
  ];

  unpackPhase = ''
    dpkg -x "$src" .
  '';

  buildInputs = with qt5; [
    libjpeg8
    libyuv
    qtwebengine
    qtx11extras
  ];

  installPhase = ''
    mkdir -p "$out"
    # use system libraries instead
    # https://github.com/NickCao/flakes/blob/ca564395aad0f2cdd45649a3769d7084a8a4fb18/pkgs/wemeet/default.nix
    rsync -rv opt/ "$out/" \
      --include "wemeet/lib/libwemeet*" \
      --include "wemeet/lib/libxnn*" \
      --include "wemeet/lib/libxcast*" \
      --include "wemeet/lib/libImSDK.so" \
      --include "wemeet/lib/libui_framework.so" \
      --include "wemeet/lib/libnxui*" \
      --include "wemeet/lib/libdesktop_common.so" \
      --include "wemeet/lib/libqt_*" \
      --include "wemeet/lib/libservice_manager.so" \
      --exclude "wemeet/lib/*" \
      --exclude "wemeet/plugins" \
      --exclude "wemeet/icons" \
      --exclude "wemeet/wemeetapp.sh" \
      --exclude "wemeet/bin/Qt*"

    mkdir -p "$out/bin"
    # TODO remove IBus and Qt style workaround
    # https://aur.archlinux.org/cgit/aur.git/commit/?h=wemeet-bin&id=32fc5d3ba55649cb1143c2b8881ba806ee14b87b
    makeQtWrapper "$out/wemeet/bin/wemeetapp" "$out/bin/wemeetapp" \
      --set-default IBUS_USE_PORTAL 1 \
      --set-default QT_STYLE_OVERRIDE fusion
    makeWrapper "$out/bin/wemeetapp" "$out/bin/wemeetapp-force-x11" \
      --set XDG_SESSION_TYPE x11 \
      --set QT_QPA_PLATFORM xcb \
      --unset WAYLAND_DISPLAY

    mkdir -p "$out/share/applications"
    install "${desktopItem}/share/applications/"*         "$out/share/applications/"
    ${
      with lib;
      if versionAtLeast (versions.majorMinor trivial.version) "22.11" then
        ''install "${desktopItemForceX11}/share/applications/"* "$out/share/applications/"''
      else
        ""
    }

    mkdir -p "$out/share"
    if [ -d opt/wemeet/icons ]; then
      cp -r opt/wemeet/icons "$out/share"
    else
      echo "directory 'opt/wemeet/icons' not found"
    fi
  '';

  passthru = {
    # TODO fix 3.19.1.400
    updateScriptEnabled = false;
    updateScript =
      let
        script = callPackage ./update.nix { };
      in
      [ "${script}/bin/update-wemeet" ];
  };

  meta = with lib; {
    homepage = "https://meeting.tencent.com";
    description = "Tencent Video Conferencing, tencent meeting";
    license = licenses.unfree;
    platforms = lib.attrNames sourceInfo;
    maintainers = with maintainers; [ brsvh ];
    mainProgram = "wemeetapp-force-x11";
  };
}
