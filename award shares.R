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

#get all-league teams
all_lg_scrape<-function(){
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
  return(all_lg)
}

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
