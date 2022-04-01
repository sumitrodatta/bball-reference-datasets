library(rvest)
library(tidyverse)
library(janitor)
library(polite)

bbref_bow=bow("https://www.basketball-reference.com/",user_agent = "Sumitro Datta",force=TRUE)
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
  a <- a %>% relocate(abbreviation, .after = "team")
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
  player_stats_a <- player_stats_a %>%
    mutate_at(vars(-c(player, tm, pos)), as.numeric) %>%
    as_tibble() %>%
    mutate(rk = season) %>%
    rename(Season = rk) %>%
    mutate(Lg = league, .before = "tm") %>%
    clean_names()
  return(player_stats_a)
}


get_season_range_player_stats<-function(seas_range=2020,league="NBA",to_scrape="per_game-team"){
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
  a <- a %>% relocate(seas_id, season, player_id, player, birth_year, hof, pos, age, experience, lg)
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
