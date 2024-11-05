{
  pkgs,
  ...
}:
{
  home = {
    packages = with pkgs; [
      wpsoffice
    ];
  };

  xdg = {
    mimeApps = {
      defaultApplications = {
        "application/msexcel" = "wps-office-et.desktop";
        "application/msword" = "wps-office-wps.desktop";
        "application/msword-template" = "wps-office-wps.desktop";
        "application/powerpoint" = "wps-office-wpp.desktop";
        "application/vnd.ms-excel" = "wps-office-et.desktop";
        "application/vnd.ms-powerpoint" = "wps-office-wpp.desktop";
        "application/vnd.ms-word" = "wps-office-wps.desktop";
        "application/vnd.mspowerpoint" = "wps-office-wpp.desktop";
        "application/wps-office.doc" = "wps-office-wps.desktop";
        "application/wps-office.docx" = "wps-office-wps.desktop";
        "application/wps-office.dot" = "wps-office-wps.desktop";
        "application/wps-office.dotx" = "wps-office-wps.desktop";
        "application/wps-office.dps" = "wps-office-wpp.desktop";
        "application/wps-office.dpt" = "wps-office-wpp.desktop";
        "application/wps-office.et" = "wps-office-et.desktop";
        "application/wps-office.ett" = "wps-office-et.desktop";
        "application/wps-office.pot" = "wps-office-wpp.desktop";
        "application/wps-office.potx" = "wps-office-wpp.desktop";
        "application/wps-office.ppt" = "wps-office-wpp.desktop";
        "application/wps-office.pptx" = "wps-office-wpp.desktop";
        "application/wps-office.wps" = "wps-office-wps.desktop";
        "application/wps-office.wpt" = "wps-office-wps.desktop";
        "application/wps-office.xls" = "wps-office-et.desktop";
        "application/wps-office.xlsx" = "wps-office-et.desktop";
        "application/wps-office.xlt" = "wps-office-et.desktop";
        "application/wps-office.xltx" = "wps-office-et.desktop";
      };
    };
  };
}
