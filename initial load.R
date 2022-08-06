library(rvest)
library(tidyverse)
library(janitor)

source("Tm & Player Seasons.R")

current_year=2022

get_all_team_stats<-function(to_scrape="per_game-team"){
  a<-tibble()
  if (to_scrape %in% c("per_poss-team", "per_poss-opponent")){
    a<-get_season_range_team_stats(seas_range = current_year:1974,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1976:1974,league="ABA",to_scrape = to_scrape))
  }
  else{
    a<-get_season_range_team_stats(seas_range = current_year:1950,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1976:1968,league="ABA",to_scrape = to_scrape)) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1949:1947,league="BAA",to_scrape = to_scrape))
  }
  return(a)
}


team_stats_base <- get_all_team_stats("totals-team")

opp_stats_base <- get_all_team_stats("totals-opponent") %>% 
  rename_with(.fn=~ paste0("opp_", .),.cols=-c(1:7))

team_stats_pg <- get_all_team_stats("per_game-team") %>% 
  rename_with(.fn= ~ paste0(., "_per_game"),.cols=-c(1:6, 10, 13, 16, 19))

opp_stats_pg <- get_all_team_stats("per_game-opponent") %>%
  rename_with(.fn=~ paste0("opp_", .),.cols=-c(1:7)) %>%
  rename_with(.fn= ~ paste0(., "_per_game"),.cols=-c(1:6, 10, 13, 16, 19))

team_stats_per_poss <- get_all_team_stats("per_poss-team") %>%
  rename_with(.fn= ~ paste0(., "_per_100_poss"),.cols=-c(1:7, 10, 13, 16, 19))

opp_stats_per_poss <- get_all_team_stats("per_poss-opponent") %>%
  rename_with(.fn=~ paste0("opp_", .),.cols=-c(1:7)) %>%
  rename_with(.fn= ~ paste0(., "_per_100_poss"),.cols=-c(1:7, 10, 13, 16, 19))

tm_summaries <- get_all_team_stats("advanced-team") %>%
  mutate(attend = gsub(",", "", attend), attend_g = gsub(",", "", attend_g)) %>%
  mutate(across(c(attend, attend_g), as.numeric))

write_csv(tm_summaries, "Data/Team Summaries.csv")
write_csv(opp_stats_per_poss, "Data/Opponent Stats Per 100 Poss.csv")
write_csv(team_stats_per_poss, "Data/Team Stats Per 100 Poss.csv")
write_csv(team_stats_base, "Data/Team Totals.csv")
write_csv(opp_stats_base, "Data/Opponent Totals.csv")
write_csv(team_stats_pg, "Data/Team Stats Per Game.csv")
write_csv(opp_stats_pg, "Data/Opponent Stats Per Game.csv")


get_all_player_stats<-function(to_scrape="totals"){
  a<-tibble()
  if (to_scrape %in% c("per_poss")){
    a<-get_season_range_player_stats(seas_range = current_year:1974,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_player_stats(seas_range = 1976:1974,league="ABA",to_scrape = to_scrape))
  }
  else if (to_scrape %in% c("shooting", "play-by-play")){
    a<-get_season_range_player_stats(seas_range = current_year:1997,league="NBA",to_scrape = to_scrape)
  }
  else{
    a<-get_season_range_player_stats(seas_range = current_year:1950,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_player_stats(seas_range = 1976:1968,league="ABA",to_scrape = to_scrape)) %>%
      bind_rows(.,get_season_range_player_stats(seas_range = 1949:1947,league="BAA",to_scrape = to_scrape))
  }
  return(a)
}


totals <- get_all_player_stats("totals")

# player season info starts with season, player, pos, age, tm, league, hof
# go into excel and add birth years for following player careers:
# players with same name in same season (example: George Johnson)
# players with same name in different seasons (example: Gerald Henderson)
# players who played seasons in multiple leagues (example: Moses Malone)

# for updating past seasons
totals_info <- totals %>%
  select(season, player:tm) %>%
  select(-experience) %>%
  arrange(season, player) %>%
  mutate(seas_id = row_number())
players_unique <- totals_info %>%
  group_by(player, birth_year) %>%
  slice(1) %>%
  arrange(seas_id) %>%
  ungroup() %>%
  mutate(player_id = row_number()) %>%
  select(player, birth_year, player_id)
totals_info <- left_join(totals_info, players_unique)
totals_info <- totals_info %>% mutate(tm = ifelse(tm == "TOT", "1TOT", tm))
removed_mult_same_yr <- totals_info %>%
  group_by(player_id, season) %>%
  arrange(tm) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(player_id) %>%
  mutate(experience = row_number())
totals_info <- left_join(totals_info, removed_mult_same_yr) %>%
  arrange(player_id, season, tm) %>%
  fill(experience) %>%
  mutate(tm = ifelse(tm == "1TOT", "TOT", tm)) %>%
  arrange(seas_id) %>%
  relocate(seas_id, player_id, .after = "season")
career_info <- totals_info %>% group_by(player_id,player,birth_year) %>% 
  summarize(hof=hof,num_seasons=max(experience),first_seas=min(season),
            last_seas=max(season)) %>% 
  ungroup() %>% group_by(player_id) %>% slice_head(n=1) %>% ungroup()
write_csv(totals_info %>% select(-hof), "Data/Player Season Info.csv")
write_csv(career_info,"Data/Player Career Info.csv")

totals <- get_all_player_stats("totals") %>% select(-hof)

per_game <- get_all_player_stats("per_game") %>% select(-hof) %>%
  rename_with(.fn= ~ paste0(., "_per_game"),.cols=-c(1:12, 16, 19, 22:23, 26))

advanced <- get_all_player_stats("advanced") %>% select(-hof) %>% select(-c(x, x_2))

per_36 <- get_all_player_stats("per_minute") %>% select(-hof) %>%
  rename_with(.fn= ~ paste0(., "_per_36_min"),.cols=-c(1:13, 16, 19, 22, 25))

per_100 <- get_all_player_stats("per_poss") %>% select(-hof) %>%
  select(-x) %>% 
  rename_with(.fn= ~ paste0(., "_per_100_poss"),.cols=-c(1:13, 16, 19, 22, 25, 35:36))

shooting <- get_all_player_stats("shooting") %>% select(-hof) %>%
  rename(avg_dist_fga = dist) %>%
  rename_with(.cols=c(15:20),.fn= ~ paste0("percent_fga_from_", ., "_range")) %>%
  rename_with(.cols=c(21:26),.fn= ~ paste0("fg_percent_from_", str_sub(., end = -3), "_range")) %>%
  rename_with(.cols=c(27:28),.fn= ~ paste0("percent_assisted_", str_sub(., end = -3), "_fg")) %>%
  rename(
    percent_dunks_of_fga = percent_fga, num_of_dunks = number,
    percent_corner_3s_of_3pa = percent_3pa, corner_3_point_percent = x3p_percent,
    num_heaves_attempted = att, num_heaves_made = number_2
  )

play_by_play <- get_all_player_stats("play-by-play") %>% select(-hof) %>%
  rename(
    on_court_plus_minus_per_100_poss = on_court,
    net_plus_minus_per_100_poss = on_off, bad_pass_turnover = bad_pass,
    lost_ball_turnover = lost_ball, shooting_foul_committed = shoot,
    offensive_foul_committed = off, shooting_foul_drawn = shoot_2,
    offensive_foul_drawn = off_2, points_generated_by_assists = pga,
    fga_blocked = blkd
  )

write_csv(advanced, "Data/Advanced.csv")
write_csv(per_100, "Data/Per 100 Poss.csv")
write_csv(per_36, "Data/Per 36 Minutes.csv")
write_csv(totals, "Data/Player Totals.csv")
write_csv(per_game, "Data/Player Per Game.csv")
write_csv(play_by_play, "Data/Player Play By Play.csv")
write_csv(shooting, "Data/Player Shooting.csv")
