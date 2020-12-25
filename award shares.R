library(rvest)
library(tidyverse)
library(janitor)

#works with mvp, roy
get_award_pcts_mvp_roy<-function(season=2020,award="mvp",lg="nba")
{
  url=paste0('https://www.basketball-reference.com/awards/awards_',season,'.html')
  awarding=award
  #aba mvp awarded from 1968-1976
  if (award=="mvp")
  {
    if (season<1977 & season>1967){
      awarding=paste0(lg,"_",award)
    }
  }
  else #aba roy has voting results from 1972 to 1975 (other years added later) 
  {
    if (season<1976 & season>1971){
      awarding=paste0(lg,"_",award)
    }
  }
  pcts =  url %>% read_html %>%
  html_nodes(xpath = paste0('//*[(@id = "div_',awarding,'")]')) %>% .[[1]] %>% html_node("table") %>% html_table()
  pcts<-pcts[,2:8] #get only voting results (no stats)
  colnames(pcts)<-pcts[1,] #first row is actual column names
  pcts<-pcts[-1,]
  rownames(pcts)<-c() #reset row names
  #add season awarded and award name
  pcts<-pcts %>% add_column(Season=season,.before="Player") %>% 
    add_column(Award=paste(lg,award),.before="Player") %>% 
    add_column(Winner=FALSE) %>% clean_names()
  return(pcts)
}
#add nba mvps
mvp<-get_award_pcts_mvp_roy()
sapply(2019:1956, function(x){
  new_seas<-get_award_pcts_mvp_roy(x,"mvp")
  mvp<<-rbind(mvp,new_seas)
})

#add aba mvps
sapply(1976:1968, function(x){
  new_seas<-get_award_pcts_mvp_roy(x,"mvp","aba")
  mvp<<-rbind(mvp,new_seas)
})

#add nba roys
roy<-get_award_pcts_mvp_roy(award = "roy")
sapply(c(2019:1964,1959), function(x){
  new_seas<-get_award_pcts_mvp_roy(x,"roy")
  roy<<-rbind(roy,new_seas)
})

#add aba roys
sapply(1975:1972, function(x){
  new_seas<-get_award_pcts_mvp_roy(x,"roy","aba")
  roy<<-rbind(roy,new_seas)
})

#add nba roy winners with no voting results
roys<- "https://www.basketball-reference.com/awards/roy.html" %>% read_html %>% 
  html_nodes(xpath='//*[(@id = "div_roy_NBA")]') %>% .[[1]] %>% html_node("table") %>% html_table()
colnames(roys)<-roys[1,]
roys<-roys[-1,]
roys<-roys %>% filter(Voting != "(V)") %>% mutate(Season=as.numeric(substr(Season,0,4))+1)
#remove asterisks (unofficial recognize)
roys<-roys %>% mutate(Player=ifelse(str_detect(Player,"\\*"),substr(Player,1,nchar(Player)-2),Player))
#remove trailing tie in 1952
roys<-roys %>% mutate(Player=ifelse(str_detect(Player,"(Tie)"),substr(Player,1,nchar(Player)-6),Player))
roys<-roys %>% select(-c(Lg,Voting,G:`WS/48`)) %>% add_column(Award="nba roy",.before="Player") %>% 
  add_column("First"=NA,"Pts Won"=NA,"Pts Max"=NA,"Share"=NA,"Winner"=TRUE) %>% clean_names()
roy<-rbind(roy,roys)

#add aba roy winners with no voting results
roys<- "https://www.basketball-reference.com/awards/roy.html" %>% read_html %>% 
  html_nodes(xpath='//*[(@id = "div_roy_ABA")]') %>% .[[1]] %>% html_node("table") %>% html_table()
colnames(roys)<-roys[1,]
roys<-roys[-1,]
roys<-roys %>% filter(Voting != "(V)") %>% mutate(Season=as.numeric(substr(Season,0,4))+1)
#1971 was a tie, so get rid of trailing (Tie) remark for Charlie Scott and Dan Issel
roys<-roys %>% mutate(Player=ifelse(str_detect(Player,"(Tie)"),substr(Player,1,nchar(Player)-6),Player))
roys<-roys %>% select(-c(Lg,Voting,G:`WS/48`)) %>% add_column(Award="aba roy",.before="Player") %>% 
  add_column("First"=NA,"Pts Won"=NA,"Pts Max"=NA,"Share"=NA,"Winner"=TRUE) %>% clean_names()
roy<-rbind(roy,roys)

rm(roys)

#works with dpoy, smoy, mip (all there are nba-only awards)
get_award_pcts_other<-function(season=2020,award="mip"){
  url=paste0('https://www.basketball-reference.com/awards/awards_',season,'.html')
  pcts =  url %>% read_html %>% html_nodes(xpath = '//comment()') %>% 
    html_text() %>% paste(collapse='') %>% read_html() %>% 
    html_node(paste0("#",award)) %>% html_table()
  pcts<-pcts[,2:8] #get only voting results (no stats)
  colnames(pcts)<-pcts[1,] #first row is actual column names
  pcts<-pcts[-1,]
  rownames(pcts)<-c() #reset row names
  #add season awarded and award name
  pcts<-pcts %>% add_column(Season=season,.before="Player") %>% 
    add_column(Award=award,.before="Player") %>% mutate(winner=FALSE) %>%
    clean_names()
  return(pcts)
}
mip<-get_award_pcts_other(2020,"mip")
seasons=c(2019:1988,1986) #no voting found for 1987
sapply(seasons, function(x){
  new_seas<-get_award_pcts_other(x,"mip")
  mip<<-rbind(mip,new_seas)
})
mip<-mip %>% add_row(season=1987,award="mip",player="Dale Ellis",age="26",tm="SEA",winner=TRUE)

dpoy<-get_award_pcts_other(2020,"dpoy")
sapply(2019:1983, function(x){
  new_seas<-get_award_pcts_other(x,"dpoy")
  dpoy<<-rbind(dpoy,new_seas)
})

smoy<-get_award_pcts_other(2020,"smoy")
seasons=2019:1984 #no voting found for 1983
sapply(seasons, function(x){
  new_seas<-get_award_pcts_other(x,"smoy")
  smoy<<-rbind(smoy,new_seas)
})
smoy<-smoy %>% add_row(season=1983,award="smoy",player="Bobby Jones",age="31",tm="PHI",winner=TRUE)

awards<-rbind(dpoy,smoy,mip,mvp,roy)
awards[,c(1,4,6:9)]<-sapply(awards[,c(1,4,6:9)],as.numeric)

winners<-awards %>% group_by(season,award) %>% slice_max(share)
awards<-anti_join(awards,winners)
winners<-winners %>% mutate(winner=TRUE)
awards<-full_join(awards,winners) %>% arrange(desc(season),award,desc(share))
psi<-read_csv("Player Season Info.csv")
awards<-awards %>% 
  mutate (player=ifelse(player=="Jaren Jackson" & season >= 2019,"Jaren Jackson Jr.",player)) %>%
  mutate (player=ifelse(player=="Marvin Bagley" & season >= 2019,"Marvin Bagley III",player)) %>%
  mutate (player=ifelse(player=="Dennis Smith" & season >= 2018,"Dennis Smith Jr.",player)) %>%
  mutate (player=ifelse(player=="Taurean Waller-Prince" & season >= 2018,"Taurean Prince",player)) %>%
  mutate (player=ifelse(player=="Tim Hardaway" & season >= 2014,"Tim Hardaway Jr.",player)) %>%
  mutate (player=ifelse(player=="Nenê Hilário" & season >= 2003,"Nenê",player))
  
awards<- left_join(awards,psi) %>% relocate(seas_id,player_id,.before=player) %>% 
  relocate(birth_year,pos,.before=age) %>% relocate(experience,.after=age) %>% select(-c(hof,lg))

write_excel_csv(awards,"Player Award Shares.csv")

#get all-league teams
all_lg<-"https://www.basketball-reference.com/awards/all_league.html" %>% read_html %>%
  html_nodes(xpath='//*[(@id = "div_awards_all_league")]') %>% html_node("table") %>% html_table() %>% .[[1]]
#change blanks to NA's and remove
all_lg[all_lg==""]=NA
all_lg<-na.omit(all_lg)
colnames(all_lg)<-c("Season","Lg","team_rank","Player1","Player2","Player3","Player4","Player5")
all_lg<-all_lg %>% mutate(Season=as.numeric(substr(Season,0,4))+1)
#players become one column
all_lg<-all_lg %>% pivot_longer(-c(Season,Lg,team_rank),names_prefix = "Player") %>% select(-name)
#separate out ties
all_lg<-all_lg %>% rename("Player"=value) %>% separate_rows(Player,sep=", ")
all_lg<-all_lg %>% mutate(Player=ifelse(str_detect(Player,"\\(T\\)$"),substr(Player,1,nchar(Player)-4),str_trim(Player)))
#separate out positions
nopos<-all_lg %>% filter(!str_detect(Player,"(C|F|G)$")) %>% mutate(Position=NA)
all_lg<-all_lg %>% filter(str_detect(Player,"(C|F|G)$")) %>% 
       mutate(Position=str_sub(Player,start=-1),Player=substr(Player,1,nchar(Player)-2))
all_lg<-rbind(all_lg,nopos)
all_lg<-all_lg %>% mutate(Type=paste("All",Lg,sep="-")) %>% select(1,2,6,3,4,5) %>% clean_names()

all_def_or_all_rookie<-function(type="all_defense")
{
  url<-paste0("https://www.basketball-reference.com/awards/",type,".html")
  alldef<-url %>% read_html %>%
    html_nodes(xpath=paste0('//*[(@id = "div_awards_',type,'")]')) %>% 
    html_node("table") %>% html_table() %>% .[[1]]
  alldef[alldef==""]=NA
  alldef<-alldef[complete.cases(alldef),]
  colnames(alldef)<-c("Season","Lg","team_rank","Player1","Player2","Player3","Player4","Player5")
  alldef<-alldef %>% mutate(Season=as.numeric(substr(Season,0,4))+1)
  alldef<-alldef %>% pivot_longer(-c(Season,Lg,team_rank),names_prefix = "Player") %>% select(-name)
  alldef<-alldef %>% rename("Player"=value) %>% separate_rows(Player,sep=", ")
  tieds<-alldef %>% 
    filter(str_detect(Player,"\\(T\\)$")) %>% mutate(Player=substr(Player,1,nchar(Player)-4))
  alldef<-alldef %>% filter(!str_detect(Player,"\\(T\\)$"))
  alldef<-rbind(alldef,tieds) %>% arrange(desc(Season),Lg,team_rank)
  if (type=="all_defense")
  {
    alldef<-alldef %>% add_column(Type="All-Defense",.before="team_rank")
  }
  else
  {
    alldef<-alldef %>% add_column(Type="All-Rookie",.before="team_rank")
  }
  alldef<-alldef %>% mutate(Position=NA) %>% clean_names()
  return(alldef)
}
alldef<-all_def_or_all_rookie()
allrook<-all_def_or_all_rookie("all_rookie")

end_seas_teams<-rbind(all_lg,alldef,allrook)

psi<-read_csv("Player Season Info.csv")
end_seas_teams<-end_seas_teams %>% 
  mutate (player=ifelse(player=="Jaren Jackson" & season >= 2019,"Jaren Jackson Jr.",player)) %>%
  mutate (player=ifelse(player=="Marvin Bagley" & season >= 2019,"Marvin Bagley III",player)) %>%
  mutate (player=ifelse(player=="Dennis Smith" & season >= 2018,"Dennis Smith Jr.",player)) %>%
  mutate (player=ifelse(player=="Tim Hardaway" & season >= 2014,"Tim Hardaway Jr.",player)) %>%
  mutate (player=ifelse(player=="Nenê Hilário" & season >= 2003,"Nenê",player))
end_seas_teams<-left_join(end_seas_teams,psi) %>% group_by(season,type,player) %>% 
  mutate(n=n()) %>% select(season:birth_year,tm,age,n)
#two George Johnsons played at same time, one won All-Defense, remove the other
end_seas_teams<-end_seas_teams %>% filter(!(player=="George Johnson" & birth_year==1956))
#if player played for multiple teams during season, keep only total stat
tots<-end_seas_teams %>% filter(n>1 & player != "George Johnson")
end_seas_teams<-anti_join(end_seas_teams,tots)
tots<-tots %>% mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,type) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm))
end_seas_teams<-full_join(end_seas_teams,tots) %>% select(-n) %>% arrange(desc(season),type,team_rank)

write_csv(end_seas_teams,"End of Season Teams.csv")

all_stars<-function(season=2020,league="NBA"){
  url=paste0("https://www.basketball-reference.com/leagues/",league,"_",season,".html")
  new_season=url %>% read_html %>% html_nodes(xpath = '//comment()') %>%
    html_text() %>% paste(collapse='') %>% read_html() %>% 
    html_nodes(xpath=paste0('//*[(@id = "div_all_star_game_rosters")]')) %>% html_nodes("table")
  team_names=new_season %>% html_nodes("caption") %>% html_text()
  rosters=new_season %>% html_nodes("tr") %>% html_text() %>% str_trim(.) %>%
    str_split(.,'\\s{2,100}')
  all_stars_tibble=tibble(player=character(),team=character(),season=integer(),lg=character())
  team1=tibble(player=rosters[[1]]) %>% mutate(team=team_names[[1]],lg=league,seas=season)
  team2=tibble(player=rosters[[2]]) %>% mutate(team=team_names[[2]],lg=league,seas=season)
  all_stars_tibble=rbind(all_stars_tibble,team1) %>% rbind(.,team2)
  return(all_stars_tibble)
}

get_all_all_stars<-function(){
  a=all_stars()
  #nba (no game in 1999)
  sapply(setdiff(2019:1951,1999),function(x){
    new_seas<-all_stars(x)
    a<<-rbind(a,new_seas)
  })
  #aba
  sapply(1976:1968,function(x){
    new_seas<-all_stars(x,"ABA")
    a<<-rbind(a,new_seas)
  })
  return(a)
}
all_stars_all_years=get_all_all_stars()
all_stars_cleaned=all_stars_all_years %>% 
  mutate(replaced=str_detect(player,"\\("),
         player=ifelse(replaced==TRUE,substr(player,1,nchar(player)-4),player),
         hof=str_detect(player,"\\*"),
         player=ifelse(hof==TRUE,substr(player,1,nchar(player)-1),player))

write_csv(all_stars_cleaned,"All-Star Selections.csv")
