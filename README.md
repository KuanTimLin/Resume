# Resume
# 林冠廷 (Tim Lin) 
- Birthday: 1999/08/03
- Education:中山大學 / 財務管理學系
- Location: New Taipei City
- E-mail: tim87754321@gmail.com
- Mobile: 0918551915

### 專業技能
- Python: 具備API提取資料能力、基礎爬蟲能力、使用cookie偽裝使用者爬蟲能力。
- R: 資料清洗、資料分析、資料視覺化、基礎統計分析。
- SQL: 資料清洗、資料分析、抓取各大公開資料庫資料。
- Excel:資料清洗及分析（xlookup,conditional formating,sorting,pivot table...etc.)、資料視覺化。
- Tableau:多種圖表資料視覺化
- 英文：Tofel 91（R:29,L:22,S:18,W:22)

* 已獲得 Google Data Analytics Certification 證書
<hr>

### 個人經驗 
-  期間 2022/11/15 - 2023/05/02 南堤消防局替代役 <BR>
   * 出勤11次 （5次救護、4次火警、1次救溺、1次大型救助）
      * 救溺出勤時成功救起民眾一名。
      * EMT1證照，CPR熟稔。
-  期間 2020/08 - 2021/08 滑板社社長<BR>
   * 訓練良好社交能力、組織活動能力
-  期間 2019 - 2020 中山海工與財管宿營公關長<BR>
-  期間 2019 - 2020 財管系籃副隊長<BR>
<hr>
   
### 相關證照
 - AWS GoogleDataAnalyticsCertification <BR>
 ![Image Alt text](https://github.com/KuanTimLin/images/blob/main/GoogleDataAnalytics.jpg)

 - Toefl <BR>
 ![Image Alt text](https://github.com/KuanTimLin/images/blob/main/托福PNG.png)
   

### 作品集 1
 ## 分析CS選手比賽數據
 - CS為目前世界上最大FPS(第一人稱射擊遊戲）電競比賽，此作品集旨在分析選手個人準度與對比賽輸贏影響力之間是否呈現正相關。
 - 個人準度指標：步槍手（暴頭率＊擊殺數/死亡數)＆狙擊手(擊殺數/死亡數）
 - 對比賽輸贏影響力：這裡使用的Rating是源自於HLTV，是一個綜和指標。數值愈大，這選手越強，有他在勝率越高。
 - 詳細Rating定義可見這支影片：https://www.youtube.com/watch?v=4rs1E4eKZcg&t=196s
   
   此處使用的資料為Kaggle上此作者的著作:(https://www.kaggle.com/datasets/naumanaarif/csgo-pro-players-dataset)
   
##使用R來處理資料

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/R%20語言1.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/R語言2.png)


#步槍手正相關圖
![Image Alt text](https://github.com/KuanTimLin/images/blob/main/步槍手.JPG)

#狙擊手正相關圖

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/狙擊手.JPG)

＃結論：不論步槍手和狙擊手，準度確實與選手在賽場上的影響力呈現正相關。

### 作品集 2
 ## 製作CS選手的KD＆Rating的Tableau圖表
 - 使用SQL確認資料完整性，並篩選所需資料
 - 將清理過的csv檔送進Tableau做視覺化
 - 圖表將選手Kill/Death從高到低排序，並使用顏色深到淺代表他們的Rating。圖表可以觀察哪位選手Kill/Death最高，Rating和Kill/Death之間的關係。

##使用SQL來處理資料

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL%201.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL2.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL3.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL4.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL5.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL6.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL7.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL8.png)


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL9.png)


#Tableau視覺化呈現
![Image Alt text](https://github.com/KuanTimLin/images/blob/main/Tableau圖表.png)



### 作品集 2
 ## 製作CS物品的市場爬蟲交易模型
  CS裡面的槍枝造型，為現在市面上最大型的遊戲虛寶交易市場。此作品集旨在建立爬蟲模型監控西方市場狀況，並且在大陸買入低價造型，賺取價差。
 - 使用Python獲取API資料
 - 將資料送進SQL處理
 - 再將處理完的資料export到Excel
 - 用Excel整理資料，做出交易決策。

 #1.使用Python獲取API資料
 
 ![Image Alt text](https://github.com/KuanTimLin/images/blob/main/Python%20API.png)

 #2.將資料送進SQL處理
 先把json檔丟入json to csv converter，轉成csv再上傳至BigQuery。
  ![Image Alt text](https://github.com/KuanTimLin/images/blob/main/SQL%20API.png)
再Export資料到EXCEL


#3.用EXCEL處理資料

先讀取CSV檔案

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/csv%E6%AA%94%E6%A1%88.png)

再從Data => From Web抓取台銀匯率資料

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/%E5%8C%AF%E7%8E%87.png)


將csv檔的資料作成PivotTable，並設定呈現的value為最小，這樣就可篩選出任意相同標的物品的底價(底價物品流通性高)。

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/pivot%20table%203.png)
  

做出簡易公式做交易決策。在底下深藍色區塊輸入自大陸買進的物品價格，上方公式會轉換成台幣並扣除手續費的金額。
在最後一格使用條件式，若淨額>0等於獲利，反之無獲利。
使用conditional formating，獲利=綠色，無獲利=紅色。


![Image Alt text](https://github.com/KuanTimLin/images/blob/main/pivottable%202.png)


依結果決定要不要買入。



 
