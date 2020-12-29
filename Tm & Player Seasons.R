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

abbrev<-read_csv("Team Abbrev.csv")

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
  a<-full_join(a,abbrev)
  a<-a %>% relocate(abbreviation,.after="team")
  return(a)
}

team_stats_base<-get_all_team_stats()

opp_stats_base<-get_all_team_stats("opponent-stats-base")
opp_stats_base<-opp_stats_base %>% rename_at(vars(-c(1:7)),~paste0("opp_",.))

team_stats_pg<-get_all_team_stats("team-stats-per_game")
team_stats_pg<-team_stats_pg %>%
  rename_at(vars(-c(1:6,10,13,16,19)),~paste0(.,"_per_game"))

opp_stats_pg<-get_all_team_stats("opponent-stats-per_game")
opp_stats_pg<-opp_stats_pg %>%
  rename_at(vars(-c(1:7)),~paste0("opp_",.)) %>%
  rename_at(vars(-c(1:6,10,13,16,19)),~paste0(.,"_per_game"))

team_stats_per_poss<-get_all_team_stats("team-stats-per_poss")
team_stats_per_poss<-team_stats_per_poss %>% 
  rename_at(vars(-c(1:7,10,13,16,19)),~paste0(.,"_per_100_poss"))

opp_stats_per_poss<-get_all_team_stats("opponent-stats-per_poss")
opp_stats_per_poss<-opp_stats_per_poss %>% 
  rename_at(vars(-c(1:7)),~paste0("opp_",.)) %>%
  rename_at(vars(-c(1:7,10,13,16,19)),~paste0(.,"_per_100_poss"))

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
  new_season<-left_join(new_season,abbrev)
  new_season<-new_season %>% relocate(abbreviation,.after="team")
  return(new_season)
}

tm_summaries<-misc_stats_scrape()
sapply(2019:1950,function(x){
  new_seas=misc_stats_scrape(x)
  tm_summaries<<-rbind(tm_summaries,new_seas)
})
sapply(1949:1947,function(x){
  new_seas=misc_stats_scrape(x,"BAA")
  tm_summaries<<-rbind(tm_summaries,new_seas)
})
sapply(1976:1968,function(x){
  new_seas=misc_stats_scrape(x,"ABA")
  tm_summaries<<-rbind(tm_summaries,new_seas)
})

write_excel_csv(tm_summaries,"Team Summaries.csv")
write_excel_csv(opp_stats_per_poss,"Opponent Stats Per 100 Poss.csv")
write_excel_csv(team_stats_per_poss,"Team Stats Per 100 Poss.csv")
write_excel_csv(team_stats_base,"Team Totals.csv")
write_excel_csv(opp_stats_base,"Opponent Totals.csv")
write_excel_csv(team_stats_pg,"Team Stats Per Game.csv")
write_excel_csv(opp_stats_pg,"Opponent Stats Per Game.csv")

all_team<-left_join(tm_summaries,team_stats_base) %>% 
  left_join(.,team_stats_per_poss) %>% left_join(.,opp_stats_base) %>% 
  left_join(.,opp_stats_per_poss)

scrape_stats <- function(season = 2017,league="NBA",type="totals"){
  #scrape
  url <- paste0("https://www.basketball-reference.com/leagues/",league,"_",season,"_",type,".html")
  stats_a <- url %>% read_html() %>% html_table() %>% .[[1]]
  
  #clean
  player_stats_a <- stats_a %>% clean_names() %>% 
    filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
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
totals<-get_all()

per_game<-get_all("per_game")
per_game<-per_game %>% rename_at(vars(-c(1:13,17,20,23:24,27)),~paste0(.,"_per_game"))

advanced<-get_all("advanced")
advanced<-advanced %>% select(-c(x,x_2))

per_36<-get_all("per_minute")
per_36<-per_36 %>% rename_at(vars(-c(1:14,17,20,23,26)),~paste0(.,"_per_36_min"))

per_100<-get_all("per_poss")
per_100<-per_100 %>% select(-x) %>% filter(season>1973)
per_100<-per_100 %>% rename_at(vars(-c(1:14,17,20,23,26,36:37)),~paste0(.,"_per_100_poss"))

all_play<-left_join(totals,per_game) %>% 
  left_join(.,per_36) %>% left_join(.,per_100) %>% left_join(.,advanced)

write_excel_csv(advanced,"Advanced.csv")
write_excel_csv(per_100,"Per 100 Poss.csv")
write_excel_csv(per_36,"Per 36 Minutes.csv")
write_excel_csv(totals,"Player Totals.csv")
write_excel_csv(per_game,"Player Per Game.csv")

#player season info starts with season, player, pos, age, tm, league, hof
#go into excel and add birth years for following player careers:
#players with same name in same season (example: George Johnson)
#players with same name in different seasons (example: Gerald Henderson)
#players who played seasons in multiple leagues (example: Moses Malone)


#for updating past seasons
totals_info=totals %>% select(season,player:tm) %>% select(-experience) %>%
  arrange(season,player) %>% mutate(seas_id=row_number())
players_unique<-totals_info %>% group_by(player,birth_year) %>% 
  slice(1) %>% arrange(seas_id) %>% ungroup() %>% 
  mutate(player_id=row_number()) %>% select(player,birth_year,player_id)
totals_info<-left_join(totals_info,players_unique)
totals_info<-totals_info %>% mutate(tm=ifelse(tm=="TOT","1TOT",tm))
removed_mult_same_yr<-totals_info %>% group_by(player_id,season) %>% 
  arrange(tm) %>% slice(1) %>% ungroup() %>% group_by(player_id) %>% 
  mutate(experience=row_number())
totals_info<-left_join(totals_info,removed_mult_same_yr) %>% 
  arrange(player_id,season,tm) %>% fill(experience) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% arrange(seas_id) %>% 
  relocate(seas_id,player_id,.after="season")
write_csv(totals_info,"Player Season Info.csv")

add_new_seas<-function(seas=2021,type="totals",update_psi=FALSE){
  a<-scrape_stats(season=seas,type=type)
  if (update_psi==TRUE){
    # no active hall of famers
    new_player_info=a %>% select(season:tm) %>% mutate(hof=FALSE) %>%
      arrange(season,player)
    psi=read_csv("Player Season Info.csv") %>% 
      select(season,player:tm) %>% 
      filter(season != seas)
    updated_psi=psi %>% add_row(new_player_info)
    #season ids
    updated_psi=updated_psi %>% mutate(seas_id=row_number())
    players_unique<-updated_psi %>% group_by(player,birth_year) %>% 
      slice(1) %>% arrange(seas_id) %>% ungroup() %>% 
      mutate(player_id=row_number()) %>% select(player,birth_year,player_id)
    updated_psi<-left_join(updated_psi,players_unique)
    #add years of experience
    updated_psi<-updated_psi %>% mutate(tm=ifelse(tm=="TOT","1TOT",tm))
    removed_mult_same_yr<-updated_psi %>% group_by(player_id,season) %>% 
      arrange(tm) %>% slice(1) %>% ungroup() %>% group_by(player_id) %>% 
      mutate(experience=row_number())
    updated_psi<-left_join(updated_psi,removed_mult_same_yr) %>% 
      arrange(player_id,season,tm) %>% fill(experience) %>% 
      mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% arrange(seas_id) %>% 
      relocate(seas_id,player_id,.after="season")
    write_csv(updated_psi,"Player Season Info.csv")
  }
  a<-left_join(a,read_csv("Player Season Info.csv")) %>% 
    relocate(seas_id,season,player_id,player,birth_year,hof,pos,age,experience,lg)
  if (type=="totals"){
    old=read_csv("Player Totals.csv") %>% filter(season != seas)
    write_csv(old %>% add_row(a),"Player Totals.csv")
  }
  else if (type=="advanced"){
    old=read_csv("Advanced.csv") %>% filter(season != seas)
    a<-a %>% select(-c(x,x_2))
    write_csv(old %>% add_row(a),"Advanced.csv")
  }
  else if (type=="per_game"){
    old=read_csv("Player Per Game.csv") %>% filter(season != seas)
    a<-a %>% rename_at(vars(-c(1:13,17,20,23:24,27)),~paste0(.,"_per_game"))
    write_csv(old %>% add_row(a),"Player Per Game.csv")
  }
  else if (type=="per_minute"){
    old=read_csv("Per 36 Minutes.csv") %>% filter(season != seas)
    a<-a %>% rename_at(vars(-c(1:14,17,20,23,26)),~paste0(.,"_per_36_min"))
    write_csv(old %>% add_row(a),"Per 36 Minutes.csv")
  }
  else if (type=="per_poss"){
    old=read_csv("Per 100 Poss.csv") %>% filter(season != seas)
    a<-a %>% select(-x) %>% 
      rename_at(vars(-c(1:14,17,20,23,26,36:37)),~paste0(.,"_per_100_poss"))
    write_csv(old %>% add_row(a),"Per 100 Poss.csv")
  }
}

add_new_seas(seas=2021,type="totals",update_psi=TRUE)
add_new_seas(seas=2021,type="advanced",update_psi=FALSE)
add_new_seas(seas=2021,type="per_game",update_psi=FALSE)
add_new_seas(seas=2021,type="per_minute",update_psi=FALSE)
add_new_seas(seas=2021,type="per_poss",update_psi=FALSE)