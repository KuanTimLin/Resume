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
   

### 作品集 
 #＃ 分析CS選手比賽數據
 - CS為目前世界上最大FPS(第一人稱射擊遊戲）電競比賽，此作品集旨在分析選手個人準度與對比賽輸贏影響力之間是否呈現正相關。
    *個人準度指標：步槍手（暴頭率＊擊殺數/死亡數)＆狙擊手(擊殺數/死亡數）
    *對比賽輸贏影響力：這裡使用的Rating是源自於HLTV，是一個綜和指標。數值愈大，這選手越強，有他在勝率越高。
    *詳細Rating定義可見這支影片：https://www.youtube.com/watch?v=4rs1E4eKZcg&t=196s
   
   此處使用的資料為Kaggle上此作者的著作:(https://www.kaggle.com/datasets/naumanaarif/csgo-pro-players-dataset)
   
#＃使用R來處理資料
＃先讀取CSV檔
csgo_players <- read.csv("~/Desktop/作品集/2/csgo_players.csv")

#判斷選手的類型，是狙擊手還是步槍手（如果使用步槍的比率/使用所有武器的比率>使用狙擊槍的比率/使用所有武器的比率，則判斷為步槍手，反之為狙擊手）
type_data <- csgo_players %>% 
  summarise(Name = nickname,
            Rifle_Percentage = rifle_kills/(rifle_kills+sniper_kills+smg_kills),
            Sniper_Percentage = sniper_kills/(rifle_kills+sniper_kills+smg_kills)) %>% 

New_Type_Data<-type_data %>% 
  mutate(Type = ifelse(Rifle_Percentage >= Sniper_Percentage,"Rifler","Sniper")) 

#篩選掉後面25%回合數不足的無效樣本
quantile(csgo_players$rounds_played)

Filter_Csgo_Player<-csgo_players %>% 
  filter(rounds_played >= 13459)

#狙擊手（狙擊手＆前75%回合數的選手）
Sniper_List<-filter(New_Type_Data,Type == "Sniper")
Filter_Sniper<-csgo_players %>% 
  filter(nickname %in% Filter_Csgo_Player$nickname) %>% 
  filter(nickname %in% Sniper_List$Name)

#步槍手（步槍手＆前75%回合數的選手）
Rifler_List<-filter(New_Type_Data,Type == "Rifler")
Filter_Rifler<-csgo_players %>% 
  filter(nickname %in% Filter_Csgo_Player$nickname) %>% 
  filter(nickname %in% Rifler_List$Name)

#狙擊手kill/death(因為狙擊手不用考慮暴頭率和助攻的問題，單純使用擊殺數/死亡數作為準度標準較為合理。)
Sniper_KD<-(Filter_Sniper$total_kills / Filter_Sniper$total_deaths)
FilteredData_Sniper<-FilteredData_Sniper %>% 
  mutate(KD = Sniper_KD)

#步槍手Kill*HeadShotRate/Death（步槍手只使用（暴頭率＊擊殺數/死亡數）作為準度指標，這樣可以減少因為隊友傷害造成的擊殺數，以及真正辨識步槍手在準度上的個人能力。）
HeadShot_Percentage<-gsub("%","",Filter_Rifler$headshot_percentage) ＃原始資料爆頭率為String格式的xx%，在這裡把它轉成數字形式。
HeadShotRate<-as.numeric(sub("%","",HeadShot_Percentage))/100

Rifler_KD<-(Filter_Rifler$total_kills * HeadShotRate)/Filter_Rifler$total_deaths

Filter_Rifler<-Filter_Rifler %>% 
  mutate(HeadShotKD = Rifler_KD)
  
#步槍手正相關圖
ggplot(data = Filter_Rifler, aes(x=HeadShotKD,y=rating)) + geom_point(color = "red")+ geom_smooth()

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/步槍手.JPG)

#狙擊手正相關圖
Filter_Sniper<-Filter_Sniper %>% 
  mutate(KD = total_kills/total_deaths)
ggplot(data = Filter_Sniper,aes(x=KD,y=rating))+geom_point(color="red")+geom_smooth()

![Image Alt text](https://github.com/KuanTimLin/images/blob/main/狙擊手.JPG)

＃結論：不論步槍手和狙擊手，準度確實與選手在賽場上的影響力呈現正相關。


      

 
