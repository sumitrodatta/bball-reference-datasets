library(rvest)
library(tidyverse)
library(janitor)
library(polite)

bbref_bow=bow("https://www.basketball-reference.com/",user_agent = "Sumitro Datta",force=TRUE,delay=10)
print(bbref_bow)

# per_game-team, per_game-opponent
# totals-team, totals-opponent
# per_poss-team, per_poss-opponent from 1974
# advanced-team
teamStats <- function(season = 2020, league = "NBA", type = "per_game-team") {
  session = nod(bbref_bow,path=paste0("leagues/", league, "_", season, ".html"))
  new_season <- scrape(session) %>% 
    html_nodes(css=paste0("#",type)) %>%
    html_table() %>% .[[1]]
  if (type=="advanced-team"){
    new_season=new_season[-c(18,23,28)]
    new_season[1, 22:25] <- as.list(paste0("opp_", new_season[1, 22:25]))
    colnames(new_season) <- new_season[1, ]
    new_season <- new_season[-1, ]
    new_season[, c(1, 3:25)] <- sapply(new_season[, c(1, 3:25)], as.numeric)
  }
  new_season <- new_season %>%
    rename(Season = Rk) %>%
    mutate(Season = season) %>%
    add_column(Lg = league, .before = "Team") %>%
    clean_names()
  return(new_season)
}


get_season_range_team_stats<-function(seas_range=2020,league="NBA",to_scrape="per_game-team"){
  a<-tibble()
  for (season in seas_range){
    new_seas<-teamStats(season=season,league=league,type=to_scrape)
    a<-bind_rows(a,new_seas)
    print(paste(season,league))
  }
  a <- a %>%
    mutate(
      playoffs = str_detect(team, "\\*"),
      team = ifelse(playoffs == TRUE, substr(team, 1, nchar(team) - 1), team)
    ) %>%
    relocate(playoffs, .after = "team")
  a <- left_join(a, read_csv("Data/Team Abbrev.csv"))
  a <- a %>% relocate(abbreviation, .after = "team") %>% arrange(desc(season),abbreviation)
  return(a)
}


scrape_stats <- function(season = 2017, league = "NBA", type = "totals") {
  # scrape
  session = nod(bbref_bow,path=paste0("leagues/", league, "_", season, "_", type, ".html"))
  stats_a <- scrape(session) %>%
    html_table() %>%
    .[[1]]
  if (type %in% c("shooting", "play-by-play")) {
    colnames(stats_a) <- stats_a[1, ]
    stats_a <- stats_a[-1, ]
  }
  # clean
  player_stats_a <- stats_a %>%
    clean_names() %>%
    filter(!player == "Player")
  if (type == "play-by-play") {
    player_stats_a <- player_stats_a %>% mutate(across(pg_percent:c_percent, ~ gsub("%", "", .)))
  }
  else if (type == "shooting") {
    player_stats_a <- player_stats_a %>% select(-starts_with("na"))
  }
  else if (type %in% c("per_minute","per_poss")) {
    player_stats_a <- player_stats_a %>% select(-e_fg_percent) #per minute added efg_percent
  }
  else if (type == "totals") {
    player_stats_a <- player_stats_a %>% select(-trp_dbl) #totals added triple double count
  }
  player_stats_a <- player_stats_a %>% 
    rename(any_of(c("tm"="team"))) %>% #upgraded tables use team
    mutate_at(vars(-c(player, tm, pos)), as.numeric) %>%
    as_tibble() %>%
    mutate(rk = season) %>%
    rename(Season = rk) %>%
    mutate(Lg = league, .before = "tm") %>%
    clean_names() %>%
    select(-any_of(c("awards"))) %>% #upgraded tables have awards
    filter(player != "League Average") %>% #upgraded tables have league average
    relocate(season,player,pos,age,lg,tm) %>% #upgraded tables rearrange columns
    mutate(tm=if_else(str_detect(tm,"[0-9]TM"),"TOT",tm)) #upgraded tables add number of teams played for
  return(player_stats_a)
}


get_season_range_player_stats<-function(seas_range=2020,league="NBA",to_scrape="per_game"){
  a<-tibble()
  for (season in seas_range){
    new_seas<-scrape_stats(season=season,league=league,type=to_scrape)
    a<-bind_rows(a,new_seas)
    print(paste(season,league))
  }
  # removed asterisk from hall of fame players
  a <- a %>%
    mutate(
      hof = str_detect(player, "\\*"),
      player = ifelse(hof, substr(player, 1, nchar(player) - 1), player)
    ) %>%
    left_join(., read_csv("Data/Player Season Info.csv"))
  a <- a %>% relocate(seas_id, season, player_id, player, birth_year, hof, pos, age, experience, lg) %>%
    arrange(desc(season),player)
  return(a) 
}


get_rookies<-function(season = 2017, league = "NBA"){
  session = nod(bbref_bow,path=paste0("leagues/", league, "_", season, "_rookies.html"))
  rookies=scrape(session) %>% html_nodes(css="#rookies") %>%
    html_table() %>% .[[1]] %>% row_to_names(1) %>% clean_names() %>% select(-rk) %>% 
    filter(!(player %in% c("","Player"))) %>% 
    mutate(season=season,.before=everything()) %>% select(season:debut) %>% 
    mutate(debut=word(debut,end=2,sep=",")) %>%
    mutate(debut=as.Date(debut,format="%b %d, '%y")) %>% arrange(player,debut)
  return(rookies)
}

get_letter_directory_table<-function(letter="/players/a/"){
  page=nod(bbref_bow,path=letter) %>% scrape()
  
  letter_table=page %>% html_table() %>% .[[1]] %>% clean_names() %>% 
    mutate(hof = str_detect(player, "\\*"),
           player = ifelse(hof, substr(player, 1, nchar(player) - 1), player),
           colleges=na_if(colleges,""),
           birth_date=mdy(birth_date)) %>%
    mutate(ht_in_in=as.numeric(word(ht,sep="-"))*12+
             as.numeric(word(ht,sep="-",start=2)),.before=wt) %>%
    select(-ht)
  
  letter_slugs=tibble(
    slug=page %>% 
      html_elements(xpath="//a") %>% html_attr("href")
  ) %>% 
    filter(str_detect(slug,"(?=.*players/)(?=.*html$)")) %>%
    slice(1:nrow(letter_table)) %>%
    mutate(slug=word(word(slug,sep="/",start=4),sep=".html"))
  
  full_letter_table=bind_cols(letter_table,letter_slugs)
  
  return(full_letter_table)
}

get_player_directory<-function(){
  session=nod(bbref_bow,path="players/")
  player_letter_hyperlinks=scrape(session) %>% 
    html_nodes("a") %>% html_attrs() %>% map_df(as_tibble_row) %>%
    filter(str_detect(href,"/players/[a-z]/$")) %>% pull(href)
  player_table=tibble()
  for (letter_link in player_letter_hyperlinks){
    letter_table=get_letter_directory_table(letter_link)
    
    player_table=bind_rows(player_table,letter_table)
    print(letter_link)
  }
  return(player_table)
}