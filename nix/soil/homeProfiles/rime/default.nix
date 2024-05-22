let
  fcitx5Config = ''
    [Hotkey]
    EnumerateWithTriggerKeys=True
    EnumerateForwardKeys=
    EnumerateBackwardKeys=
    EnumerateSkipFirst=False
    EnumerateGroupForwardKeys=
    EnumerateGroupBackwardKeys=

    [Hotkey/TriggerKeys]
    0=Super+space
    1=Zenkaku_Hankaku
    2=Hangul

    [Hotkey/AltTriggerKeys]
    0=Shift_L

    [Hotkey/ActivateKeys]
    0=Hangul_Hanja

    [Hotkey/DeactivateKeys]
    0=Hangul_Romaja

    [Hotkey/PrevPage]
    0=Up

    [Hotkey/NextPage]
    0=Down

    [Hotkey/PrevCandidate]
    0=Shift+Tab

    [Hotkey/NextCandidate]
    0=Tab

    [Hotkey/TogglePreedit]
    0=Control+Alt+P

    [Behavior]
    ActiveByDefault=True
    ShareInputState=No
    PreeditEnabledByDefault=True
    ShowInputMethodInformation=True
    showInputMethodInformationWhenFocusIn=False
    CompactInputMethodInformation=True
    ShowFirstInputMethodInformation=True
    DefaultPageSize=8
    OverrideXkbOption=False
    CustomXkbOption=
    EnabledAddons=
    DisabledAddons=
    PreloadInputMethod=True
    AllowInputMethodForPassword=False
    ShowPreeditForPassword=False
    AutoSavePeriod=30
  '';

  fcitx5Profile = ''
    [Groups/0]
    Name=Default
    Default Layout=us
    DefaultIM=rime

    [Groups/0/Items/0]
    Name=rime
    Layout=

    [GroupOrder]
    0=Default
  '';

  lunaPinyinSimpConfig = ''
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

  rimeCustomConfig = ''
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
    dataFile = {
      "fcitx5/config".text = fcitx5Config;
      "fcitx5/profile".text = fcitx5Profile;
      "fcitx5/rime/default.custom.yaml".text = rimeCustomConfig;
      "fcitx5/rime/luna_pinyin_simp.custom.yaml".text = lunaPinyinSimpConfig;
    };
  };
}
