# Data-coverage
資料流程
For Terrestrial vertebrates

***
# 蒐集物種時空分布資料
## 針對陸域脊椎動物
- 來源資料集：
  1. 為公開資料可免費取得
  2. 資訊須包含調查時間與&調查地點(不接受座標模糊化)
- 蒐集資訊：
    Species, Year, Month, Longitude, Latitude, Event ID, (Individual count)

## 匯入資料集
1. 國家公園(~2016)
2. eBird (2000/01~2017/12) (ebd)
3. 中華民國野鳥學會資料庫CWBF (2017) (CWBF) 
4. 臺灣動物路死觀察網TaiRON (~2016/10/18) (RK)
* 全球生物多樣性資訊機構GBIF (~2018)
   5. Breeding Bird Survey2009-2015(BBS)                                                                        
   6. 第四次森林資源調查野生動物調查錄音檔案監聽辨識及資料分析(Forest.4th)                                            
   7. 嘉義縣阿里山鄉中大型哺乳動物相對豐度與分布調查暨各部落傳統文化祭儀中野生動物之利用及當代狩獵範圍之探討(CY.ML)
   8. 臺灣兩棲類資源調查與教育宣導推廣計畫(Frog)
* 臺灣生物多樣性資訊機構TaiBIF (~2018) 
   9. 101年度鰲鼓濕地森林園區鳥類監測及建立監測模式(Augu101)
   10. 102年度鰲鼓濕地森林園區鳥類監測及建立監測模式(Augu102)
   11. 外來種斑腿樹蛙控制與監測計畫(Inv)
   12. 花蓮縣平地造林區森林性動物監測計畫(HLC)

## Summary
### Raw data???
|Project    |     #Record|#Species|
|-----------|------------|--------|
|    Augu101|       14719|
|    Augu102|        8542|
|        BBS|      231117|
|       CWBF|     1780254|
|      CY.ML|        8954|
|        ebd|      977694|
| Forest.4th|        6483|
|       Frog|       62757|
|        HLC|        1512|
|        Inv|         835|
|         RK|       22567|(22553)|
|        TBN|       60027|
|    **Sum**| **3175447**| 

### D:/Chun/Analysis/RRR/Dataset/data_20180417.RData???
|    Project| #Record|#Species|
|-----------|--------|--------|
|    Augu101|   14719|
|    Augu102|    8542|
|        BBS|  231117|
|       CWBF| 1780254|
|      CY.ML|    8954|
|        ebd|  977694|
| Forest.4th|    6483|
|       Frog|   62757|
|        HLC|    1512|
|        Inv|     835|
|         NP|  213413|
|         RK|   22503|
|        TBN|   60026|
|    **Sum**| **3388809**| 

***
# 來源資料集蒐集
* 比對 TaiBNET 名錄
資料量：3,230,521
物種數： 832

* 篩選台灣資料(不含澎金馬) 
資料量： 3,016,645
物種數： 815

* 針對台灣原生物種

