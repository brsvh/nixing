#  overlays.proprius-fonts

## Usage

Simply use this overlay `brsvh.overlays.proprius-fonts` when import `nixpkgs`, and then install packages this overlay provided.

``` nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/unstable";
    brsvh.url = "github:brsvh/nixing";
    brsvh.inputs.nixpkgs.follows = "nixpkgs";
  };

  # OPTIONAL use my cachix binary cache.
  nixConfig = {
    extra-substituters = [ "https://brsvh.cachix.org" ];
    extra-trusted-public-keys = [ "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE=" ];
  };

  outputs =
    { nixpkgs, ... }@inputs:
    {
      nixosConfiguraitons.YOUR-CONFIGURATION = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (
            { pkgs, ... }:
            {
              nixpkgs = {
                config.allowUnfree = true;
                overlays = [ brsvh.overlays.proprius-fonts ];
              };

              environment.systemPackageswith = with pkgs; [
                tsangertypeFonts.gratisProCommercium
                tsangertypeFonts.gratisProPersona
              ];
            }
          )
        ];
      };
    };
}
```

## Scaffolds

### `pkgs.tsangertypeFonts.combine`

- Type: `combine :: [Derivation] -> Derivation`.
- Input: A list of font derivations.
- Output: A linked derivation of input.

Combine multiple font derivations into a single derivation, named `tsangertype-combined-fonts`.

### `pkgs.tsangertypeFonts.combine'`

- Type: `combine' :: [Derivation] -> { name: String } -> Derivation`.
- Input:
  1. A list of font derivations.
  2. An attrsets must include `name`.
- Output: A linked derivation of input.

Combine multiple font derivations into a single derivation, with specified name.

### `pkgs.tsangertypeFonts.listAllFonts`

- Type: `listAllFonts :: [Derivation]`.

Return a list of all TsangerType Fonts.

### `pkgs.tsangertypeFonts.listFontsWithCond`

- Type: `listFontsWithCond :: {Derivation, ...} -> (Derivation -> Bool) -> [Derivation]`
- Input:
  1. A list of font derivations.
  2. A predicate use to filtering derivations.
- Output: A list of font derivations that satisfy predicate checking.

Return a list of font derivations from a given list of font derivations that meet the specified conditions.

### `pkgs.tsangertypeFonts.listGratisProCommerciumFonts`

- Type: `listGratisProCommerciumFonts :: {Derivation, ...} -> [Derivation]`

Return a list of font derivation from a given list of font derivations that gratis propria commercium.

### `pkgs.tsangertypeFonts.listGratisProPersonaFonts`

- Type: `listGratisProPersonaFonts :: {Derivation, ...} -> [Derivation]`

Return a list of font derivation from a given list of font derivations that gratis propria persona.

## Packages

### `pkgs.tsangertypeFonts.gratisProCommercium`

All TsangerType fonts that permit commercial use.

It is link to all the following fonts.

| Package                                                   | Font name        |
|-----------------------------------------------------------|------------------|
| `tsangertypeFonts.tsangertype-feibai-w01-font`            | 仓耳非白W01      |
| `tsangertypeFonts.tsangertype-feibai-w02-font-font`       | 仓耳非白W02      |
| `tsangertypeFonts.tsangertype-feibai-w03-font`            | 仓耳非白W03      |
| `tsangertypeFonts.tsangertype-feibai-w04-font`            | 仓耳非白W04      |
| `tsangertypeFonts.tsangertype-feibai-w05-font`            | 仓耳非白W05      |
| `tsangertypeFonts.tsangertype-shuyuan-w01-font`           | 仓耳舒圆体W01    |
| `tsangertypeFonts.tsangertype-shuyuan-w02-font`           | 仓耳舒圆体W02    |
| `tsangertypeFonts.tsangertype-shuyuan-w03-font`           | 仓耳舒圆体W03    |
| `tsangertypeFonts.tsangertype-shuyuan-w04-font`           | 仓耳舒圆体W04    |
| `tsangertypeFonts.tsangertype-shuyuan-w05-font`           | 仓耳舒圆体W05    |
| `tsangertypeFonts.tsangertype-xiaowanzi-font`             | 仓耳小丸子       |
| `tsangertypeFonts.tsangertype-yumo-w01-font`              | 仓耳与墨W01      |
| `tsangertypeFonts.tsangertype-yumo-w02-font`              | 仓耳与墨W02      |
| `tsangertypeFonts.tsangertype-yumo-w03-font`              | 仓耳与墨W03      |
| `tsangertypeFonts.tsangertype-yumo-w04-font`              | 仓耳与墨W04      |
| `tsangertypeFonts.tsangertype-yumo-w05-font`              | 仓耳与墨W05      |
| `tsangertypeFonts.tsangertype-yuyang-w01-font`            | 仓耳渔阳体W01    |
| `tsangertypeFonts.tsangertype-yuyang-w02-font`            | 仓耳渔阳体W02    |
| `tsangertypeFonts.tsangertype-yuyang-w03-font`            | 仓耳渔阳体W03    |
| `tsangertypeFonts.tsangertype-yuyang-w04-font`            | 仓耳渔阳体W04    |
| `tsangertypeFonts.tsangertype-yuyang-w05-font`            | 仓耳渔阳体W05    |
| `tsangertypeFonts.tsangertypey-zhoukezhengdabangshu-font` | 仓耳周珂正大榜书 |

### `pkgs.tsangertypeFonts.gratisProCommercium`

All TsangerType fonts that only permit personal use.

It is link to all the following fonts.

| Package                                                         | Font name              |
|-----------------------------------------------------------------|------------------------|
| `tsangertypeFonts.tsangertype-aidekunkun-font`                  | 仓耳爱的坤坤体         |
| `tsangertypeFonts.tsangertype-aiminxiaokai-font`                | 仓耳爱民小楷           |
| `tsangertypeFonts.tsangertype-aiminxingkai-font`                | 仓耳爱民行楷           |
| `tsangertypeFonts.tsangertype-aiminxingshu-font`                | 仓耳爱民行书           |
| `tsangertypeFonts.tsangertype-ainimengmengda-font`              | 仓耳爱你萌萌哒         |
| `tsangertypeFonts.tsangertype-aiqinglianxisheng-font`           | 仓耳爱情练习生         |
| `tsangertypeFonts.tsangertype-babilong-font`                    | 仓耳吧吡咙体           |
| `tsangertypeFonts.tsangertype-bailing-w01-font`                 | 仓耳百灵W01            |
| `tsangertypeFonts.tsangertype-bailing-w02-font`                 | 仓耳百灵W02            |
| `tsangertypeFonts.tsangertype-bailing-w03-font`                 | 仓耳百灵W03            |
| `tsangertypeFonts.tsangertype-bailing-w04-font`                 | 仓耳百灵W04            |
| `tsangertypeFonts.tsangertype-bailing-w05-font`                 | 仓耳百灵W05            |
| `tsangertypeFonts.tsangertype-banghei-font`                     | 仓耳榜黑               |
| `tsangertypeFonts.tsangertype-bantangshouzha-font`              | 仓耳半糖手札           |
| `tsangertypeFonts.tsangertype-benmiaozaici-font`                | 仓耳本喵在此体         |
| `tsangertypeFonts.tsangertype-cangermuxi-font`                  | 仓耳木兮体             |
| `tsangertypeFonts.tsangertype-caolulinshouji-font`              | 仓耳曹录林手迹         |
| `tsangertypeFonts.tsangertype-caozhebinjunxiukaishu-font`       | 仓耳曹哲斌俊秀楷书     |
| `tsangertypeFonts.tsangertype-caozhebinjunxiukaishu-bold-font`  | 仓耳曹哲斌俊秀楷书加粗 |
| `tsangertypeFonts.tsangertype-caozhebinkaishu-font`             | 仓耳曹哲斌楷书         |
| `tsangertypeFonts.tsangertype-caozhebinkaishu-bold-font`        | 仓耳曹哲斌楷书加粗     |
| `tsangertypeFonts.tsangertype-caozhebinlingyunkaishu-font`      | 仓耳曹哲斌灵韵楷书     |
| `tsangertypeFonts.tsangertype-caozhebinlingyunkaishu-bold-font` | 仓耳曹哲斌灵韵楷书加粗 |
| `tsangertypeFonts.tsangertype-caozhebinxingkai-font`            | 仓耳曹哲斌行楷         |
| `tsangertypeFonts.tsangertype-chengshishangkongdefanxing-font`  | 城市上空的繁星体       |
| `tsangertypeFonts.tsangertype-chuangyi-w01-font`                | 仓耳创意体W01          |
| `tsangertypeFonts.tsangertype-chuangyi-w02-font`                | 仓耳创意体W02          |
| `tsangertypeFonts.tsangertype-chuangyi-w03-font`                | 仓耳创意体W03          |
| `tsangertypeFonts.tsangertype-chunfeng-font`                    | 仓耳春风体             |
| `tsangertypeFonts.tsangertype-chuyu-font`                       | 仓耳初遇体             |
| `tsangertypeFonts.tsangertype-daimengxiaomutou-font`            | 仓耳呆萌小木头体       |
| `tsangertypeFonts.tsangertype-dainiushouxie-font`               | 呆牛手写体             |
| `tsangertypeFonts.tsangertype-daji-font`                        | 仓耳大吉体             |
| `tsangertypeFonts.tsangertype-damanman-w01-font`                | 仓耳大漫漫体W01        |
| `tsangertypeFonts.tsangertype-damanman-w02-font`                | 仓耳大漫漫体W02        |
| `tsangertypeFonts.tsangertype-damanman-w03-font`                | 仓耳大漫漫体W03        |
| `tsangertypeFonts.tsangertype-damanman-w04-font`                | 仓耳大漫漫体W04        |
| `tsangertypeFonts.tsangertype-damanman-w05-font`                | 仓耳大漫漫体W05        |
| `tsangertypeFonts.tsangertype-daofengzhanshi-font`              | 刀锋战士体             |
| `tsangertypeFonts.tsangertype-daofengzhanshi-bold-font`         | 刀锋战士粗体           |
| `tsangertypeFonts.tsangertype-dianshichengjin-font`             | 仓耳点石成金体         |
| `tsangertypeFonts.tsangertype-diewu-font`                       | 仓耳蝶舞体             |
| `tsangertypeFonts.tsangertype-diyiyanaishangni-font`            | 仓耳第一眼爱上你       |
| `tsangertypeFonts.tsangertype-dubai-font`                       | 仓耳独白体             |
| `tsangertypeFonts.tsangertype-dudu-w01-font`                    | 仓耳嘟嘟体W01          |
| `tsangertypeFonts.tsangertype-dudu-w02-font`                    | 仓耳嘟嘟体W02          |
| `tsangertypeFonts.tsangertype-dudu-w03-font`                    | 仓耳嘟嘟体W03          |
| `tsangertypeFonts.tsangertype-dudu-w04-font`                    | 仓耳嘟嘟体W04          |
| `tsangertypeFonts.tsangertype-fanghei-font`                     | 仓耳方黑               |
| `tsangertypeFonts.tsangertype-feifei-w01-font`                  | 仓耳飞飞体W01          |
| `tsangertypeFonts.tsangertype-feifei-w02-font`                  | 仓耳飞飞体W02          |
| `tsangertypeFonts.tsangertype-feifei-w03-font`                  | 仓耳飞飞体W03          |
| `tsangertypeFonts.tsangertype-feifei-w04-font`                  | 仓耳飞飞体W04          |
| `tsangertypeFonts.tsangertype-feiteng-font`                     | 仓耳沸腾体             |
| `tsangertypeFonts.tsangertype-fengerchui-font`                  | 仓耳风儿吹             |
| `tsangertypeFonts.tsangertype-fenghei-font`                     | 仓耳丰黑               |
| `tsangertypeFonts.tsangertype-fengwujiutian-w01-font`           | 仓耳锋舞九天W01        |
| `tsangertypeFonts.tsangertype-fengwujiutian-w02-font`           | 仓耳锋舞九天W02        |
| `tsangertypeFonts.tsangertype-fengwujiutian-w03-font`           | 仓耳锋舞九天W03        |
| `tsangertypeFonts.tsangertype-fengwujiutian-w04-font`           | 仓耳锋舞九天W04        |
| `tsangertypeFonts.tsangertype-fengwujiutian-w05-font`           | 仓耳锋舞九天W05        |
| `tsangertypeFonts.tsangertype-fengwujiutian-w06-font`           | 仓耳锋舞九天W06        |
| `tsangertypeFonts.tsangertype-fengyun-w01-font`                 | 仓耳锋韵W01            |
| `tsangertypeFonts.tsangertype-fengyun-w02-font`                 | 仓耳锋韵W02            |
| `tsangertypeFonts.tsangertype-fengyun-w03-font`                 | 仓耳锋韵W03            |
| `tsangertypeFonts.tsangertype-fengyun-w04-font`                 | 仓耳锋韵W04            |
| `tsangertypeFonts.tsangertype-fengyun-w05-font`                 | 仓耳锋韵W05            |
| `tsangertypeFonts.tsangertype-fengyun-w06-font`                 | 仓耳锋韵W06            |
| `tsangertypeFonts.tsangertype-fengyun-w07-font`                 | 仓耳锋韵W07            |
| `tsangertypeFonts.tsangertype-gexingtuya-font`                  | 仓耳个性涂鸦体         |
| `tsangertypeFonts.tsangertype-guateng-font`                     | 仓耳瓜藤体             |
| `tsangertypeFonts.tsangertype-gufengkaishu-font`                | 仓耳古风楷书           |
| `tsangertypeFonts.tsangertype-gufengxingshu-font`               | 仓耳古风行书           |
| `tsangertypeFonts.tsangertype-guli-w01-font`                    | 仓耳谷力W01            |
| `tsangertypeFonts.tsangertype-guli-w02-font`                    | 仓耳谷力W02            |
| `tsangertypeFonts.tsangertype-guli-w03-font`                    | 仓耳谷力W03            |
| `tsangertypeFonts.tsangertype-guli-w04-font`                    | 仓耳谷力W04            |
| `tsangertypeFonts.tsangertype-guli-w05-font`                    | 仓耳谷力W05            |
| `tsangertypeFonts.tsangertype-guodongxixi-font`                 | 仓耳果冻吸吸体         |
| `tsangertypeFonts.tsangertype-guyun-font`                       | 仓耳古韵体             |
| `tsangertypeFonts.tsangertype-haloutuxiansheng-font`            | 仓耳哈喽兔先生体       |
| `tsangertypeFonts.tsangertype-hanshan-font`                     | 仓耳寒山体             |
| `tsangertypeFonts.tsangertype-hefeng-font`                      | 仓耳荷风体             |
| `tsangertypeFonts.tsangertype-huayu-font`                       | 仓耳花语               |
| `tsangertypeFonts.tsangertype-jiaotangmaqiduo-font`             | 仓耳焦糖玛奇朵体       |
| `tsangertypeFonts.tsangertype-jiaxuan-font`                     | 仓耳佳轩体             |
| `tsangertypeFonts.tsangertype-jiekou-font`                      | 仓耳结扣体             |
| `tsangertypeFonts.tsangertype-jiemo-font`                       | 仓耳芥末体             |
| `tsangertypeFonts.tsangertype-jingya-font`                      | 仓耳静雅体             |
| `tsangertypeFonts.tsangertype-jinkai-01-w01-font`               | 仓耳今楷01-W01         |
| `tsangertypeFonts.tsangertype-jinkai-01-w02-font`               | 仓耳今楷01-W02         |
| `tsangertypeFonts.tsangertype-jinkai-01-w03-font`               | 仓耳今楷01-W03         |
| `tsangertypeFonts.tsangertype-jinkai-01-w04-font`               | 仓耳今楷01-W04         |
| `tsangertypeFonts.tsangertype-jinkai-01-w05-font`               | 仓耳今楷01-W05         |
| `tsangertypeFonts.tsangertype-jinkai-02-w01-font`               | 仓耳今楷02-W01         |
| `tsangertypeFonts.tsangertype-jinkai-02-w02-font`               | 仓耳今楷02-W02         |
| `tsangertypeFonts.tsangertype-jinkai-02-w03-font`               | 仓耳今楷02-W03         |
| `tsangertypeFonts.tsangertype-jinkai-02-w04-font`               | 仓耳今楷02-W04         |
| `tsangertypeFonts.tsangertype-jinkai-02-w05-font`               | 仓耳今楷02-W05         |
| `tsangertypeFonts.tsangertype-jinkai-03-w01-font`               | 仓耳今楷03-W01         |
| `tsangertypeFonts.tsangertype-jinkai-03-w02-font`               | 仓耳今楷03-W02         |
| `tsangertypeFonts.tsangertype-jinkai-03-w03-font`               | 仓耳今楷03-W03         |
| `tsangertypeFonts.tsangertype-jinkai-03-w04-font`               | 仓耳今楷03-W04         |
| `tsangertypeFonts.tsangertype-jinkai-03-w05-font`               | 仓耳今楷03-W05         |
| `tsangertypeFonts.tsangertype-jinkai-04-w01-font`               | 仓耳今楷04-W01         |
| `tsangertypeFonts.tsangertype-jinkai-04-w02-font`               | 仓耳今楷04-W02         |
| `tsangertypeFonts.tsangertype-jinkai-04-w03-font`               | 仓耳今楷04-W03         |
| `tsangertypeFonts.tsangertype-jinkai-04-w04-font`               | 仓耳今楷04-W04         |
| `tsangertypeFonts.tsangertype-jinkai-04-w05-font`               | 仓耳今楷04-W05         |
| `tsangertypeFonts.tsangertype-jinkai-05-w01-font`               | 仓耳今楷05-W01         |
| `tsangertypeFonts.tsangertype-jinkai-05-w02-font`               | 仓耳今楷05-W02         |
| `tsangertypeFonts.tsangertype-jinkai-05-w03-font`               | 仓耳今楷05-W03         |
| `tsangertypeFonts.tsangertype-jinkai-05-w04-font`               | 仓耳今楷05-W04         |
| `tsangertypeFonts.tsangertype-jinkai-05-w05-font`               | 仓耳今楷05-W05         |
| `tsangertypeFonts.tsangertype-jinshirongyao-font`               | 仓耳金石荣耀体         |
| `tsangertypeFonts.tsangertype-jundongkaishu-font`               | 仓耳俊冬楷书           |
| `tsangertypeFonts.tsangertype-jundongxingshu-font`              | 仓耳俊冬行书           |
| `tsangertypeFonts.tsangertype-juzhenchangfang-font`             | 仓耳聚珍长仿           |
| `tsangertypeFonts.tsangertype-juziwei-font`                     | 仓耳橘子味             |
| `tsangertypeFonts.tsangertype-keke-font`                        | 仓耳柯柯体             |
| `tsangertypeFonts.tsangertype-konglukaishu-font`                | 仓耳孔璐楷书           |
| `tsangertypeFonts.tsangertype-lankai-font`                      | 仓耳兰楷               |
| `tsangertypeFonts.tsangertype-leizhenhanfeng-font`              | 仓耳雷震汉风体         |
| `tsangertypeFonts.tsangertype-lekeke-font`                      | 仓耳乐可可体           |
| `tsangertypeFonts.tsangertype-lelebiji-font`                    | 乐乐笔记体             |
| `tsangertypeFonts.tsangertype-lezhi-font`                       | 仓耳乐志体             |
| `tsangertypeFonts.tsangertype-lianaibiji-font`                  | 仓耳恋爱笔记体         |
| `tsangertypeFonts.tsangertype-lifei-font`                       | 仓耳利飞体             |
| `tsangertypeFonts.tsangertype-linggang-w01-font`                | 仓耳凌刚体W01          |
| `tsangertypeFonts.tsangertype-linggang-w02-font`                | 仓耳凌刚体W02          |
| `tsangertypeFonts.tsangertype-linggang-w03-font`                | 仓耳凌刚体W03          |
| `tsangertypeFonts.tsangertype-linggang-w04-font`                | 仓耳凌刚体W04          |
| `tsangertypeFonts.tsangertype-linggang-w05-font`                | 仓耳凌刚体W05          |
| `tsangertypeFonts.tsangertype-lingyun-font`                     | 仓耳凌云体             |
| `tsangertypeFonts.tsangertype-lishi-font`                       | 仓耳力士体             |
| `tsangertypeFonts.tsangertype-liyuan-font`                      | 仓耳丽圆               |
| `tsangertypeFonts.tsangertype-manmiao-font`                     | 仓耳曼妙体             |
| `tsangertypeFonts.tsangertype-maobohe-font`                     | 仓耳猫薄荷体           |
| `tsangertypeFonts.tsangertype-meixin-w01-font`                  | 仓耳美心体W01          |
| `tsangertypeFonts.tsangertype-meixin-w02-font`                  | 仓耳美心体W02          |
| `tsangertypeFonts.tsangertype-mengbaokeji-font`                 | 仓耳萌宝柯基体         |
| `tsangertypeFonts.tsangertype-mengdaimiaowei-font`              | 仓耳萌呆喵尾体         |
| `tsangertypeFonts.tsangertype-mengdie-font`                     | 仓耳梦蝶体             |
| `tsangertypeFonts.tsangertype-mengduoduo-font`                  | 仓耳萌朵朵体           |
| `tsangertypeFonts.tsangertype-mengmengdaxiaodouzi-font`         | 萌萌哒小豆子体         |
| `tsangertypeFonts.tsangertype-mengmiaojiang-font`               | 仓耳萌喵酱             |
| `tsangertypeFonts.tsangertype-mengtonglishu-font`               | 仓耳梦桐隶书           |
| `tsangertypeFonts.tsangertype-mengxingdelibie-font`             | 仓耳梦醒的离别体       |
| `tsangertypeFonts.tsangertype-minghei-w01-font`                 | 仓耳明黑W01            |
| `tsangertypeFonts.tsangertype-minghei-w02-font`                 | 仓耳明黑W02            |
| `tsangertypeFonts.tsangertype-minghei-w03-font`                 | 仓耳明黑W03            |
| `tsangertypeFonts.tsangertype-minghei-w04-font`                 | 仓耳明黑W04            |
| `tsangertypeFonts.tsangertype-minghei-w05-font`                 | 仓耳明黑W05            |
| `tsangertypeFonts.tsangertype-minghei-w06-font`                 | 仓耳明黑W06            |
| `tsangertypeFonts.tsangertype-minghei-w07-font`                 | 仓耳明黑W07            |
| `tsangertypeFonts.tsangertype-minghei-w08-font`                 | 仓耳明黑W08            |
| `tsangertypeFonts.tsangertype-mingkai-w01-font`                 | 仓耳明楷W01            |
| `tsangertypeFonts.tsangertype-mingkai-w02-font`                 | 仓耳明楷W02            |
| `tsangertypeFonts.tsangertype-mingkai-w03-font`                 | 仓耳明楷W03            |
| `tsangertypeFonts.tsangertype-mingkai-w04-font`                 | 仓耳明楷W04            |
| `tsangertypeFonts.tsangertype-mingyue-font`                     | 仓耳明月体             |
| `tsangertypeFonts.tsangertype-mizhiguo-font`                    | 仓耳秘之果             |
| `tsangertypeFonts.tsangertype-mocha-font`                       | 仓耳抹茶体             |
| `tsangertypeFonts.tsangertype-nihaoshiguang-font`               | 仓耳你好时光体         |
| `tsangertypeFonts.tsangertype-nishiwodequanshijie-font`         | 你是我的全世界         |
| `tsangertypeFonts.tsangertype-nishiwoweiyi-font`                | 仓耳你是我唯一         |
| `tsangertypeFonts.tsangertype-nuannanshouzha-font`              | 仓耳暖男手札体         |
| `tsangertypeFonts.tsangertype-nuanxin-font`                     | 仓耳暖心体             |
| `tsangertypeFonts.tsangertype-nvwang-font`                      | 仓耳女王体             |
| `tsangertypeFonts.tsangertype-peiban-font`                      | 仓耳陪伴体             |
| `tsangertypeFonts.tsangertype-piaomiao-font`                    | 仓耳缥缈体             |
| `tsangertypeFonts.tsangertype-qiaole-w01-font`                  | 仓耳巧乐W01            |
| `tsangertypeFonts.tsangertype-qiaole-w02-font`                  | 仓耳巧乐W02            |
| `tsangertypeFonts.tsangertype-qiaole-w03-font`                  | 仓耳巧乐W03            |
| `tsangertypeFonts.tsangertype-qiaole-w04-font`                  | 仓耳巧乐W04            |
| `tsangertypeFonts.tsangertype-qiaole-w05-font`                  | 仓耳巧乐W05            |
| `tsangertypeFonts.tsangertype-qiedoudou-font`                   | 仓耳企鹅豆豆体         |
| `tsangertypeFonts.tsangertype-qiming-font`                      | 仓耳启明体             |
| `tsangertypeFonts.tsangertype-qingchunyiniweiming-font`         | 仓耳青春以你为名体     |
| `tsangertypeFonts.tsangertype-qingfeng-font`                    | 仓耳清风体             |
| `tsangertypeFonts.tsangertype-qingfengxieyang-font`             | 仓耳轻风斜阳体         |
| `tsangertypeFonts.tsangertype-qinghe-w01-font`                  | 仓耳青禾体W01          |
| `tsangertypeFonts.tsangertype-qinghe-w02-font`                  | 仓耳青禾体W02          |
| `tsangertypeFonts.tsangertype-qinghe-w03-font`                  | 仓耳青禾体W03          |
| `tsangertypeFonts.tsangertype-qinghe-w04-font`                  | 仓耳青禾体W04          |
| `tsangertypeFonts.tsangertype-qinghe-w05-font`                  | 仓耳青禾体W05          |
| `tsangertypeFonts.tsangertype-qinghuan-font`                    | 仓耳清欢体             |
| `tsangertypeFonts.tsangertype-qingji-font`                      | 仓耳青吉体             |
| `tsangertypeFonts.tsangertype-qingmei-font`                     | 仓耳青梅体             |
| `tsangertypeFonts.tsangertype-qingning-font`                    | 仓耳青柠体             |
| `tsangertypeFonts.tsangertype-qingqiuxiaojiu-font`              | 仓耳青丘小九           |
| `tsangertypeFonts.tsangertype-qingque-font`                     | 仓耳青雀体             |
| `tsangertypeFonts.tsangertype-qingsong-font`                    | 仓耳青宋               |
| `tsangertypeFonts.tsangertype-qingxin-font`                     | 仓耳晴心体             |
| `tsangertypeFonts.tsangertype-qingyou-font`                     | 仓耳轻悠体             |
| `tsangertypeFonts.tsangertype-qinqinmeizhuang-font`             | 亲亲美妆体             |
| `tsangertypeFonts.tsangertype-qiuyue-font`                      | 仓耳秋月体             |
| `tsangertypeFonts.tsangertype-quancunzuikeai-font`              | 仓耳全村最可爱         |
| `tsangertypeFonts.tsangertype-quhei-font`                       | 仓耳趣黑               |
| `tsangertypeFonts.tsangertype-rouhei-font`                      | 仓耳柔黑               |
| `tsangertypeFonts.tsangertype-ruihei-font`                      | 仓耳锐黑               |
| `tsangertypeFonts.tsangertype-runhei-w01-font`                  | 仓耳润黑W01            |
| `tsangertypeFonts.tsangertype-runhei-w02-font`                  | 仓耳润黑W02            |
| `tsangertypeFonts.tsangertype-runhei-w03-font`                  | 仓耳润黑W03            |
| `tsangertypeFonts.tsangertype-shangshanruoshui-font`            | 仓耳上善若水           |
| `tsangertypeFonts.tsangertype-shenniaoxinshengchuangyi-font`    | 神鸟新生创意体         |
| `tsangertypeFonts.tsangertype-shenqidedoudou-font`              | 仓耳神奇的豆豆体       |
| `tsangertypeFonts.tsangertype-shuhei-font`                      | 仓耳曙黑               |
| `tsangertypeFonts.tsangertype-sirou-font`                       | 仓耳丝柔体             |
| `tsangertypeFonts.tsangertype-siyaoxingkai-font`                | 仓耳丝摇行楷           |
| `tsangertypeFonts.tsangertype-siyecaodexingfu-font`             | 仓耳四叶草的幸福体     |
| `tsangertypeFonts.tsangertype-songguo-font`                     | 松果体                 |
| `tsangertypeFonts.tsangertype-suxin-font`                       | 仓耳苏心体             |
| `tsangertypeFonts.tsangertype-tianmimi-font`                    | 仓耳甜蜜蜜体           |
| `tsangertypeFonts.tsangertype-tianmu-w01-font`                  | 仓耳天沐体W01          |
| `tsangertypeFonts.tsangertype-tianmu-w02-font`                  | 仓耳天沐体W02          |
| `tsangertypeFonts.tsangertype-tianmu-w03-font`                  | 仓耳天沐体W03          |
| `tsangertypeFonts.tsangertype-tianmu-w04-font`                  | 仓耳天沐体W04          |
| `tsangertypeFonts.tsangertype-tianmu-w05-font`                  | 仓耳天沐体W05          |
| `tsangertypeFonts.tsangertype-tianqunxingkai-font`              | 仓耳天群行楷           |
| `tsangertypeFonts.tsangertype-tiansuolele-font`                 | 仓耳天锁乐乐体         |
| `tsangertypeFonts.tsangertype-tingfeng-font`                    | 仓耳听风体             |
| `tsangertypeFonts.tsangertype-tongzhuo-font`                    | 仓耳同桌体             |
| `tsangertypeFonts.tsangertype-tuya-w01-font`                    | 仓耳涂鸦体W01          |
| `tsangertypeFonts.tsangertype-tuya-w02-font`                    | 仓耳涂鸦体W02          |
| `tsangertypeFonts.tsangertype-tuya-w03-font`                    | 仓耳涂鸦体W03          |
| `tsangertypeFonts.tsangertype-wandongxingshu-font`              | 仓耳万东行书           |
| `tsangertypeFonts.tsangertype-weilairiji-font`                  | 未来日记体             |
| `tsangertypeFonts.tsangertype-wugexingbuqingchun-font`          | 仓耳无个性不青春体     |
| `tsangertypeFonts.tsangertype-wuliangshoufu-font`               | 仓耳无量寿福体         |
| `tsangertypeFonts.tsangertype-xiangzuozouxiangyouzou-font`      | 仓耳向左走向右走体     |
| `tsangertypeFonts.tsangertype-xiaobaikaishu-font`               | 仓耳小白楷书           |
| `tsangertypeFonts.tsangertype-xiaobaiwan-font`                  | 仓耳小百万             |
| `tsangertypeFonts.tsangertype-xiaobaixingshu-font`              | 仓耳小白行书           |
| `tsangertypeFonts.tsangertype-xiaodianer-font`                  | 仓耳小点儿体           |
| `tsangertypeFonts.tsangertype-xiaofang-font`                    | 仓耳小方体             |
| `tsangertypeFonts.tsangertype-xiaokeai-w01-font`                | 仓耳小可爱体W01        |
| `tsangertypeFonts.tsangertype-xiaokeai-w02-font`                | 仓耳小可爱体W02        |
| `tsangertypeFonts.tsangertype-xiaokeai-w03-font`                | 仓耳小可爱体W03        |
| `tsangertypeFonts.tsangertype-xiaokeai-w04-font`                | 仓耳小可爱体W04        |
| `tsangertypeFonts.tsangertype-xiaokeai-w05-font`                | 仓耳小可爱体W05        |
| `tsangertypeFonts.tsangertype-xiaokeai-w06-font`                | 仓耳小可爱体W06        |
| `tsangertypeFonts.tsangertype-xiaomanman-w01-font`              | 仓耳小漫漫体W01        |
| `tsangertypeFonts.tsangertype-xiaomanman-w02-font`              | 仓耳小漫漫体W02        |
| `tsangertypeFonts.tsangertype-xiaomanman-w03-font`              | 仓耳小漫漫体W03        |
| `tsangertypeFonts.tsangertype-xiaomanman-w04-font`              | 仓耳小漫漫体W04        |
| `tsangertypeFonts.tsangertype-xiaomanman-w05-font`              | 仓耳小漫漫体W05        |
| `tsangertypeFonts.tsangertype-feigexiaosaxingshu-font`          | 飞哥潇洒行书           |
| `tsangertypeFonts.tsangertype-xiaoshipin-font`                  | 仓耳小视频体           |
| `tsangertypeFonts.tsangertype-xiaowoniudemeng-font`             | 仓耳小蜗牛的梦         |
| `tsangertypeFonts.tsangertype-xiaoxiaohuochai-font`             | 仓耳小小火柴体         |
| `tsangertypeFonts.tsangertype-xiaoxiaomangguo-font`             | 仓耳小小芒果体         |
| `tsangertypeFonts.tsangertype-xiaoyaoxingshu-font`              | 仓耳逍遥行书           |
| `tsangertypeFonts.tsangertype-xiaoyashouji-font`                | 仓耳小雅手迹           |
| `tsangertypeFonts.tsangertype-xiaoyutongxue-font`               | 仓耳小雨同学体         |
| `tsangertypeFonts.tsangertype-xiayizhanxingfu-font`             | 仓耳下一站幸福体       |
| `tsangertypeFonts.tsangertype-xingchenkaishu-font`              | 仓耳星辰楷书           |
| `tsangertypeFonts.tsangertype-xingfuyuezhang-font`              | 仓耳幸福乐章体         |
| `tsangertypeFonts.tsangertype-xinghansong-font`                 | 仓耳星汉宋             |
| `tsangertypeFonts.tsangertype-xinxing-font`                     | 仓耳心星体             |
| `tsangertypeFonts.tsangertype-xinyan-font`                      | 仓耳新颜体             |
| `tsangertypeFonts.tsangertype-xinyue01-font`                    | 仓耳新悦体             |
| `tsangertypeFonts.tsangertype-xinyue02-font`                    | 仓耳欣月体             |
| `tsangertypeFonts.tsangertype-xiuxianxingshu-font`              | 仓耳绣线行书           |
| `tsangertypeFonts.tsangertype-xiuzhen-font`                     | 仓耳秀蓁体             |
| `tsangertypeFonts.tsangertype-xiyuan-font`                      | 仓耳细圆体             |
| `tsangertypeFonts.tsangertype-xuansan-01-w01-font`              | 仓耳玄三01-W01         |
| `tsangertypeFonts.tsangertype-xuansan-01-w02-font`              | 仓耳玄三01-W02         |
| `tsangertypeFonts.tsangertype-xuansan-01-w03-font`              | 仓耳玄三01-W03         |
| `tsangertypeFonts.tsangertype-xuansan-01-w04-font`              | 仓耳玄三01-W04         |
| `tsangertypeFonts.tsangertype-xuansan-01-w05-font`              | 仓耳玄三01-W05         |
| `tsangertypeFonts.tsangertype-xuansan-02-w01-font`              | 仓耳玄三02-W01         |
| `tsangertypeFonts.tsangertype-xuansan-02-w02-font`              | 仓耳玄三02-W02         |
| `tsangertypeFonts.tsangertype-xuansan-02-w03-font`              | 仓耳玄三02-W03         |
| `tsangertypeFonts.tsangertype-xuansan-02-w04-font`              | 仓耳玄三02-W04         |
| `tsangertypeFonts.tsangertype-xuansan-02-w05-font`              | 仓耳玄三02-W05         |
| `tsangertypeFonts.tsangertype-xuansan-03-w01-font`              | 仓耳玄三03-W01         |
| `tsangertypeFonts.tsangertype-xuansan-03-w02-font`              | 仓耳玄三03-W02         |
| `tsangertypeFonts.tsangertype-xuansan-03-w03-font`              | 仓耳玄三03-W03         |
| `tsangertypeFonts.tsangertype-xuansan-03-w04-font`              | 仓耳玄三03-W04         |
| `tsangertypeFonts.tsangertype-xuansan-03-w05-font`              | 仓耳玄三03-W05         |
| `tsangertypeFonts.tsangertype-xuansan-04-w01-font`              | 仓耳玄三04-W01         |
| `tsangertypeFonts.tsangertype-xuansan-04-w02-font`              | 仓耳玄三04-W02         |
| `tsangertypeFonts.tsangertype-xuansan-04-w03-font`              | 仓耳玄三04-W03         |
| `tsangertypeFonts.tsangertype-xuansan-04-w04-font`              | 仓耳玄三04-W04         |
| `tsangertypeFonts.tsangertype-xuansan-04-w05-font`              | 仓耳玄三04-W05         |
| `tsangertypeFonts.tsangertype-xuansan-m-w01-font`               | 仓耳玄三M-W01          |
| `tsangertypeFonts.tsangertype-xuansan-m-w02-font`               | 仓耳玄三M-W02          |
| `tsangertypeFonts.tsangertype-xuansan-m-w03-font`               | 仓耳玄三M-W03          |
| `tsangertypeFonts.tsangertype-xuansan-m-w04-font`               | 仓耳玄三M-W04          |
| `tsangertypeFonts.tsangertype-xuansan-m-w05-font`               | 仓耳玄三M-W05          |
| `tsangertypeFonts.tsangertype-xunmengshilitaohua-font`          | 寻梦十里桃花体         |
| `tsangertypeFonts.tsangertype-xunzhaomoxianshouxie-font`        | 寻找魔仙手写体         |
| `tsangertypeFonts.tsangertype-yangguangmingmei-font`            | 仓耳阳光明媚体         |
| `tsangertypeFonts.tsangertype-yangming-font`                    | 仓耳阳明体             |
| `tsangertypeFonts.tsangertype-yaoguangxingshu-font`             | 仓耳尧光行书           |
| `tsangertypeFonts.tsangertype-yasong-font`                      | 仓耳雅宋               |
| `tsangertypeFonts.tsangertype-yayue-font`                       | 仓耳雅月体             |
| `tsangertypeFonts.tsangertype-yezhiling-font`                   | 仓耳叶之灵体           |
| `tsangertypeFonts.tsangertype-yihei-w01-font`                   | 仓耳逸黑W01            |
| `tsangertypeFonts.tsangertype-yihei-w02-font`                   | 仓耳逸黑W02            |
| `tsangertypeFonts.tsangertype-yihei-w03-font`                   | 仓耳逸黑W03            |
| `tsangertypeFonts.tsangertype-yihei-w04-font`                   | 仓耳逸黑W04            |
| `tsangertypeFonts.tsangertype-yihei-w05-font`                   | 仓耳逸黑W05            |
| `tsangertypeFonts.tsangertype-yinghe-font`                      | 仓耳硬核体             |
| `tsangertypeFonts.tsangertype-yiranyizha-font`                  | 仓耳易燃易炸体         |
| `tsangertypeFonts.tsangertype-yishibuding-font`                 | 仓耳意式布丁体         |
| `tsangertypeFonts.tsangertype-yisong-font`                      | 仓耳宜宋               |
| `tsangertypeFonts.tsangertype-yiyechunfeng-font`                | 仓耳一夜春风体         |
| `tsangertypeFonts.tsangertype-youran-font`                      | 仓耳悠然体             |
| `tsangertypeFonts.tsangertype-yuanbao-font`                     | 仓耳元宝体             |
| `tsangertypeFonts.tsangertype-yucheng-w01-font`                 | 仓耳羽辰体W01          |
| `tsangertypeFonts.tsangertype-yucheng-w02-font`                 | 仓耳羽辰体W02          |
| `tsangertypeFonts.tsangertype-yucheng-w03-font`                 | 仓耳羽辰体W03          |
| `tsangertypeFonts.tsangertype-yucheng-w04-font`                 | 仓耳羽辰体W04          |
| `tsangertypeFonts.tsangertype-yucheng-w05-font`                 | 仓耳羽辰体W05          |
| `tsangertypeFonts.tsangertype-yudong-font`                      | 仓耳悦动体             |
| `tsangertypeFonts.tsangertype-yueman-font`                      | 仓耳月满体             |
| `tsangertypeFonts.tsangertype-yukai-font`                       | 仓耳玉楷               |
| `tsangertypeFonts.tsangertype-yule-font`                        | 仓耳鱼乐体             |
| `tsangertypeFonts.tsangertype-yunhaisongtao-font`               | 仓耳云海松涛体         |
| `tsangertypeFonts.tsangertype-yunhei-w01-font`                  | 仓耳云黑-W01           |
| `tsangertypeFonts.tsangertype-yunhei-w02-font`                  | 仓耳云黑-W02           |
| `tsangertypeFonts.tsangertype-yunhei-w03-font`                  | 仓耳云黑-W03           |
| `tsangertypeFonts.tsangertype-yunhei-w04-font`                  | 仓耳云黑-W04           |
| `tsangertypeFonts.tsangertype-yunhei-w05-font`                  | 仓耳云黑-W05           |
| `tsangertypeFonts.tsangertype-yunhei-w06-font`                  | 仓耳云黑-W06           |
| `tsangertypeFonts.tsangertype-yunhei-w07-font`                  | 仓耳云黑-W07           |
| `tsangertypeFonts.tsangertype-yunhei-w08-font`                  | 仓耳云黑-W08           |
| `tsangertypeFonts.tsangertype-yunxuan-font`                     | 仓耳云轩体             |
| `tsangertypeFonts.tsangertype-yuxiaoxiao-font`                  | 仓耳雨潇潇体           |
| `tsangertypeFonts.tsangertype-zaijiannaxienian-font`            | 仓耳再见那些年体       |
| `tsangertypeFonts.tsangertype-zengguofan-font`                  | 仓耳曾国藩体           |
| `tsangertypeFonts.tsangertype-zhangyuxiaowanzi-font`            | 仓耳章鱼小丸子体       |
| `tsangertypeFonts.tsangertype-zhenzhu-font`                     | 仓耳珍珠体             |
| `tsangertypeFonts.tsangertype-zhiqu-w01-font`                   | 仓耳知曲体W01          |
| `tsangertypeFonts.tsangertype-zhiqu-w02-font`                   | 仓耳知曲体W02          |
| `tsangertypeFonts.tsangertype-zhiqu-w03-font`                   | 仓耳知曲体W03          |
| `tsangertypeFonts.tsangertype-zhiqu-w04-font`                   | 仓耳知曲体W04          |
| `tsangertypeFonts.tsangertype-zhiqu-w05-font`                   | 仓耳知曲体W05          |
| `tsangertypeFonts.tsangertype-zhixiangkanzheni-font`            | 仓耳只想看着你         |
| `tsangertypeFonts.tsangertype-zhixin-font`                      | 仓耳知新体             |
| `tsangertypeFonts.tsangertype-zhiyuxiwenqing-font`              | 仓耳治愈系文青         |
| `tsangertypeFonts.tsangertype-zhouxinyoulong-font`              | 仓耳周鑫游龙体         |
| `tsangertypeFonts.tsangertype-zhuangyuankai-font`               | 仓耳状元楷             |
| `tsangertypeFonts.tsangertype-zhuyan-font`                      | 仓耳竹言体             |
| `tsangertypeFonts.tsangertype-zongheng-font`                    | 仓耳纵横体             |
