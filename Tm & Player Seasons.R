library(rvest)
library(tidyverse)
library(janitor)

#team-stats-base
#opponent-stats-base
#team-stats-per_poss from 1974
#opponent-stats-per_poss from 1974
teamStats<-function(season=2020,league="NBA",type="team-stats-base"){
  url=paste0("https://www.basketball-reference.com/leagues/",league,"_",season,".html")
  new_season=url %>% read_html %>% html_nodes(xpath = '//comment()') %>%
    html_text() %>% paste(collapse='') %>% read_html() %>% 
    html_nodes(xpath=paste0('//*[(@id = "div_',type,'")]')) %>% 
    html_nodes("table") %>% .[[1]] %>% html_table()
  new_season<-new_season %>% rename(Season=Rk) %>% mutate(Season=season) %>% 
    add_column(Lg=league,.before="Team") %>% clean_names()
  return(new_season)
}

get_all_team_stats<-function(to_scrape="team-stats-base")
{
  a<-teamStats(2020,type=to_scrape)
  if (to_scrape %in% c("team-stats-per_poss","opponent-stats-per_poss"))
  {
    #nba
    sapply(2019:1974,function(x){
      new_seas<-teamStats(x,type=to_scrape)
      a<<-rbind(a,new_seas)
    })
    #aba
    sapply(1976:1974,function(x){
      new_seas<-teamStats(x,"ABA",type=to_scrape)
      a<<-rbind(a,new_seas)
    })
  }
  else
  {
    #nba
    sapply(2019:1950,function(x){
      new_seas<-teamStats(x,type=to_scrape)
      a<<-rbind(a,new_seas)
    })
    #aba
    sapply(1976:1968,function(x){
      new_seas<-teamStats(x,"ABA",type=to_scrape)
      a<<-rbind(a,new_seas)
    })
    #baa
    sapply(1949:1947,function(x){
      new_seas<-teamStats(x,"BAA",type=to_scrape)
      a<<-rbind(a,new_seas)
    })
  }
  a<- a %>%
    mutate(playoffs=str_detect(team,"\\*"),
           team=ifelse(playoffs==TRUE,substr(team,1,nchar(team)-1),team)) %>%
    relocate(playoffs,.after="team")
  a<-full_join(a,read_csv("Team Abbrev.csv"))
  a<-a %>% relocate(abbreviation,.after="team")
  return(a)
}

misc_stats_scrape<-function(season=2020,league="NBA"){
  url=paste0("https://www.basketball-reference.com/leagues/",league,"_",season,".html")
  new_season=url %>% read_html %>% html_nodes(xpath = '//comment()') %>%
    html_text() %>% paste(collapse='') %>% read_html() %>% 
    html_nodes(xpath='//*[(@id = "div_misc_stats")]') %>% 
    html_nodes("table") %>% .[[1]] %>% html_table()
  new_season[1,22:25]=paste0("opp_",new_season[1,22:25])
  colnames(new_season)<-new_season[1,]
  new_season<-new_season[-1,]
  new_season[,c(1,3:25)]<-sapply(new_season[,c(1,3:25)],as.numeric)
  new_season<-new_season %>% rename(Season=Rk) %>% mutate(Season=season) %>% 
    add_column(Lg=league,.before="Team") %>% 
    mutate(Playoffs=str_detect(Team,"\\*"),
           Team=ifelse(Playoffs==TRUE,substr(Team,1,nchar(Team)-1),Team)) %>%
    relocate(Playoffs,.after="Team") %>% clean_names()
  return(new_season)
}

scrape_stats <- function(season = 2017,league="NBA",type="totals"){
  #scrape
  url <- paste0("https://www.basketball-reference.com/leagues/",league,"_",season,"_",type,".html")
  stats_a <- url %>% read_html() %>% html_table() %>% .[[1]]
  if (type %in% c("shooting","play-by-play")){
    colnames(stats_a)<-stats_a[1,]
    stats_a<-stats_a[-1,]
  }
  #clean
  player_stats_a <- stats_a %>% clean_names() %>% 
    filter(!player=="Player")
  if (type=="play-by-play"){
    player_stats_a<-player_stats_a %>% mutate(across(pg_percent:c_percent,~gsub("%","",.)))
  }
  else if (type=="shooting"){
    player_stats_a=player_stats_a %>% select(-starts_with("na"))
  }
  player_stats_a <- player_stats_a %>% mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    as_tibble() %>% 
    mutate(rk=season) %>% rename(Season=rk) %>% mutate(Lg=league,.before="tm") %>% clean_names()
  return(player_stats_a)
}

get_all<-function(to_scrape="totals")
{
  a<-scrape_stats(2020,type=to_scrape)
  if (to_scrape=="per_poss")
  {
    #nba
    sapply(2019:1974,function(x){
      new_seas<-scrape_stats(x,type=to_scrape)
      names(new_seas)<-names(a)
      a<<-rbind(a,new_seas)
    })
    #aba
    sapply(1976:1974,function(x){
      new_seas<-scrape_stats(x,"ABA",type=to_scrape)
      names(new_seas)<-names(a)
      a<<-rbind(a,new_seas)
    })
  }
  else if (to_scrape %in% c("shooting","play-by-play")){
    sapply(2019:1997,function(x){
      new_seas=scrape_stats(x,type=to_scrape)
      names(new_seas)<-names(a)
      a<<-rbind(a,new_seas)
    })
  }
  else
  {
      #nba
      sapply(2019:1950,function(x){
        new_seas<-scrape_stats(x,type=to_scrape)
        names(new_seas)<-names(a)
        a<<-rbind(a,new_seas)
      })
      #aba
      sapply(1976:1968,function(x){
        new_seas<-scrape_stats(x,"ABA",type=to_scrape)
        names(new_seas)<-names(a)
        a<<-rbind(a,new_seas)
      })
      #baa
      sapply(1949:1947,function(x){
        new_seas<-scrape_stats(x,"BAA",type=to_scrape)
        names(new_seas)<-names(a)
        a<<-rbind(a,new_seas)
      })
  }
  #removed asterisk from hall of fame players and add as separate column
  a<-a %>% 
    mutate(hof=str_detect(player,"\\*"),
           player=ifelse(hof==TRUE,substr(player,1,nchar(player)-1),player)) %>%
    left_join(.,read_csv("Player Season Info.csv"))
  a<-a %>% relocate(seas_id,season,player_id,player,birth_year,hof,pos,age,experience,lg)
  return(a)
}