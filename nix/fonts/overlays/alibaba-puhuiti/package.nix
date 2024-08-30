{
  fetchzip,
  lib,
  stdenvNoCC,
}:
let
  alibaba-puhuiti-license = {
    free = false;
    fullName = "《阿里巴巴普惠体3.0版》法律声明";
    redistributable = true;
    shortName = "alibaba-puhuiti-license";
    url = "https://www.yuque.com/yiguang-wkqc2/puhuiti/nus9wiinq4aeiegy";
  };
in
stdenvNoCC.mkDerivation {
  pname = "alibaba-puhuiti";
  version = "3.0";

  src = fetchzip {
    hash = "sha256-7ZmurN+sC3binkhABG588Tdvz7KLc06+UvK5TOTjqvY=";
    stripRoot = false;
    url = "https://puhuiti.oss-cn-hangzhou.aliyuncs.com/AlibabaPuHuiTi-3.zip";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype

    cp -r $src/AlibabaPuHuiTi-3 $out/share/fonts/truetype/

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://www.alibabafonts.com";
    description = "An Chinese font gratis propria commercium";
    license = alibaba-puhuiti-license;
    platforms = platforms.all;
    maintainers = with maintainers; [ brsvh ];
  };
}