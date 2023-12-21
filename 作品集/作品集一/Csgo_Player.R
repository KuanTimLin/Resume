csgo_players <- read.csv("~/Desktop/作品集/2/csgo_players.csv")

#判斷選手的類型，是狙擊手還是步槍手
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

#狙擊手
Sniper_List<-filter(New_Type_Data,Type == "Sniper")
Filter_Sniper<-csgo_players %>% 
  filter(nickname %in% Filter_Csgo_Player$nickname) %>% 
  filter(nickname %in% Sniper_List$Name)

#步槍手
Rifler_List<-filter(New_Type_Data,Type == "Rifler")
Filter_Rifler<-csgo_players %>% 
  filter(nickname %in% Filter_Csgo_Player$nickname) %>% 
  filter(nickname %in% Rifler_List$Name)

#狙擊手kill/death,rating
Sniper_KD<-(Filter_Sniper$total_kills / Filter_Sniper$total_deaths)
FilteredData_Sniper<-FilteredData_Sniper %>% 
  mutate(KD = Sniper_KD)

#步槍手KD
HeadShot_Percentage<-gsub("%","",Filter_Rifler$headshot_percentage)
HeadShotRate<-as.numeric(sub("%","",HeadShot_Percentage))/100

Rifler_KD<-(Filter_Rifler$total_kills * HeadShotRate)/Filter_Rifler$total_deaths

Filter_Rifler<-Filter_Rifler %>% 
  mutate(HeadShotKD = Rifler_KD)
#步槍手圖
ggplot(data = Filter_Rifler, aes(x=HeadShotKD,y=rating)) + geom_point(color = "red")+ geom_smooth()


#狙擊手KD
Filter_Sniper<-Filter_Sniper %>% 
  mutate(KD = total_kills/total_deaths)
ggplot(data = Filter_Sniper,aes(x=KD,y=rating))+geom_point(color="red")+geom_smooth()


