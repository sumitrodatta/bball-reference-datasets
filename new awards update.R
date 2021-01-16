library(rvest)
library(tidyverse)
library(janitor)

source("award shares.R")

mvp=get_award_pcts_mvp_roy(season=2021,award = "mvp")
roy=get_award_pcts_mvp_roy(season=2021,award = "roy")
mip=get_award_pcts_other(season=2021,award = "mip")
dpoy=get_award_pcts_other(season=2021,award = "mip")
smoy=get_award_pcts_other(season=2021,award = "smoy")

new_seas_awards=rbind(dpoy,smoy,mip,mvp,roy)

new_seas_awards[,c(1,4,6:9)]<-sapply(new_seas_awards[,c(1,4,6:9)],as.numeric)

winners<-new_seas_awards %>% group_by(award) %>% slice_max(share)
new_seas_awards<-anti_join(new_seas_awards,winners)
winners<-winners %>% mutate(winner=TRUE)
new_seas_awards<-full_join(new_seas_awards,winners) %>% arrange(award,desc(share))
psi<-read_csv("Player Season Info.csv")
new_seas_awards<-new_seas_awards %>% 
  mutate (player=ifelse(player=="Jaren Jackson" & season >= 2019,"Jaren Jackson Jr.",player)) %>%
  mutate (player=ifelse(player=="Marvin Bagley" & season >= 2019,"Marvin Bagley III",player)) %>%
  mutate (player=ifelse(player=="Dennis Smith" & season >= 2018,"Dennis Smith Jr.",player)) %>%
  mutate (player=ifelse(player=="Taurean Waller-Prince" & season >= 2018,"Taurean Prince",player)) %>%
  mutate (player=ifelse(player=="Tim Hardaway" & season >= 2014,"Tim Hardaway Jr.",player)) %>%
  mutate (player=ifelse(player=="Nenê Hilário" & season >= 2003,"Nenê",player))

new_seas_awards<- left_join(new_seas_awards,psi) %>% relocate(seas_id,player_id,.before=player) %>% 
  relocate(birth_year,pos,.before=age) %>% relocate(experience,.after=age) %>% select(-c(hof,lg))

old_awards=read_csv("Player Award Shares.csv")
write_csv(old_awards %>% add_row(new_seas_awards),"Player Award Shares.csv")

new_seas_allstars=all_stars(season=2021) %>% 
  mutate(replaced=str_detect(player,"\\("),
         player=ifelse(replaced==TRUE,substr(player,1,nchar(player)-4),player),
         hof=str_detect(player,"\\*"),
         player=ifelse(hof==TRUE,substr(player,1,nchar(player)-1),player))

write_csv(read_csv("All-Star Selections.csv") %>% add_row(new_seas_allstars),"All-Star Selections.csv")

