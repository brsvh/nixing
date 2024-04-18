{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
with lib;
let
  rime = ''
    patch:
      schema_list:
        - schema: "luna_pinyin_simp"
      ascii_composer/good_old_caps_lock: true
      ascii_composer/switch_key/Caps_Lock: "commit_text"
      ascii_composer/switch_key/Shift_L: "commit_code"
      ascii_composer/switch_key/Shift_R: "commit_code"

      switcher/caption: "【设置菜单】"
      switcher/option_list_separator: "/"
      switcher/hotkeys:
        - "F4"
      menu/page_size: 8
      key_binder/bindings:
        - accept: "Control+p"
          send: "Up"
          when: "composing"
        - accept: "Control+n"
          send: "Down"
          when: "composing"
        - accept: "Control+b"
          send: "Left"
          when: "composing"
        - accept: "Control+f"
          send: "Right"
          when: "composing"
        - accept: "Alt+b"
          send: "Shift+Left"
          when: "composing"
        - accept: "Alt+f"
          send: "Shift+Right"
          when: "composing"
        - accept: "Control+a"
          send: "Home"
          when: "composing"
        - accept: "Control+e"
          send: "End"
          when: "composing"
        - accept: "Control+d"
          send: "Delete"
          when: "composing"
        - accept: "Control+h"
          send: "BackSpace"
          when: "composing"
        - accept: "Alt+h"
          send: "Shift+BackSpace"
          when: "composing"
        - accept: "Control+g"
          send: "Escape"
          when: "composing"
        - accept: "Control+bracketleft"
          send: "Escape"
          when: "composing"
        - accept: "Alt+v"
          send: "Page_Up"
          when: "composing"
        - accept: "Control+v"
          send: "Page_Down"
          when: "composing"
        - accept: "Tab"
          send: "Down"
          when: "has_menu"
        - accept: "ISO_Left_Tab"
          send: "Up"
          when: "has_menu"
        - accept: "Shift+Tab"
          send: "Up"
          when: "has_menu"
        - accept: "minus"
          send: "Page_Up"
          when: "has_menu"
        - accept: "equal"
          send: "Page_Down"
          when: "has_menu"
        - accept: "bracketleft"
          send: "Page_Up"
          when: "has_menu"
        - accept: "bracketright"
          send: "Page_Down"
          when: "has_menu"
  '';

  ibus = ''
    patch:
      style/horizontal: true
      style/inline_preedit: false
  '';

  luna_pinyin_simp = ''
    patch:punctuator/half_shape:
      "!": "！"
      '"':
        pair:
          - "“"
          - "”"
      "#": "#"
      "$":
        - "￥"
        - "$"
        - "€"
        - "～"
        - "×"
        - "÷"
        - "°"
        - "℃"
        - "‰"
        - "‱"
        - "℉"
        - "©"
        - "®"
      "%": "%"
      "&": "&"
      "'":
        pair:
          - "‘"
          - "’"
      "*": "*"
      "+": "+"
      ",": "，"
      "-": "-"
      ".": "。"
      "/": "/"
      "\\": "、"
      ":": "："
      ";": "；"
      "=": "="
      "?": "？"
      "@": "@"
      "(": "（"
      ")": "）"
      "[": "【"
      "]": "】"
      "{": "「"
      "}": "」"
      "<": "《"
      ">": "》"
      "^": "……"
      "_": "——"
      "`": "`"
      "|": "·"
      "~": "~"
    recognizer/patterns/reverse_lookup:

  '';
in
{
  i18n = {
    inputMethod = {
      enabled = "fcitx5";

      fcitx5 = {
        addons = with pkgs; [ fcitx5-rime ];
      };
    };
  };

  xdg = {
    configFile = {
      "ibus/rime/default.custom.yaml".text = rime;
      "ibus/rime/ibus_rime.custom.yaml".text = ibus;
      "ibus/rime/luna_pinyin_simp.custom.yaml".text = luna_pinyin_simp;
    };

    dataFile = {
      "fcitx5/rime/default.custom.yaml".text = rime;
      "fcitx5/rime/luna_pinyin_simp.custom.yaml".text = luna_pinyin_simp;
    };
  };
}
