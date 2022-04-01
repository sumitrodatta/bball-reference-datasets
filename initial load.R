library(rvest)
library(tidyverse)
library(janitor)

source("Tm & Player Seasons.R")

get_all_team_stats<-function(to_scrape="per_game-team"){
  a<-tibble()
  if (to_scrape %in% c("per_poss-team", "per_poss-opponent")){
    a<-get_season_range_team_stats(seas_range = 2020:1974,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1976:1974,league="ABA",to_scrape = to_scrape))
  }
  else{
    a<-get_season_range_team_stats(seas_range = 2020:1950,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1976:1968,league="ABA",to_scrape = to_scrape)) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1950:1947,league="BAA",to_scrape = to_scrape))
  }
  return(a)
}


team_stats_base <- get_all_team_stats("totals-team")

opp_stats_base <- get_all_team_stats("totals-opponent")
opp_stats_base <- opp_stats_base %>% rename_at(vars(-c(1:7)), ~ paste0("opp_", .))

team_stats_pg <- get_all_team_stats("per_game-team")
team_stats_pg <- team_stats_pg %>%
  rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))

opp_stats_pg <- get_all_team_stats("per_game-opponent")
opp_stats_pg <- opp_stats_pg %>%
  rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
  rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))

team_stats_per_poss <- get_all_team_stats("per_poss-team")
team_stats_per_poss <- team_stats_per_poss %>%
  rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))

opp_stats_per_poss <- get_all_team_stats("per_poss-opponent")
opp_stats_per_poss <- opp_stats_per_poss %>%
  rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
  rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))

tm_summaries <- get_all_team_stats("advanced-team")
tm_summaries <- tm_summaries %>%
  mutate(attend = gsub(",", "", attend), attend_g = gsub(",", "", attend_g)) %>%
  mutate(across(c(attend, attend_g), as.numeric))

write_excel_csv(tm_summaries, "Data/Team Summaries.csv")
write_excel_csv(opp_stats_per_poss, "Data/Opponent Stats Per 100 Poss.csv")
write_excel_csv(team_stats_per_poss, "Data/Team Stats Per 100 Poss.csv")
write_excel_csv(team_stats_base, "Data/Team Totals.csv")
write_excel_csv(opp_stats_base, "Data/Opponent Totals.csv")
write_excel_csv(team_stats_pg, "Data/Team Stats Per Game.csv")
write_excel_csv(opp_stats_pg, "Data/Opponent Stats Per Game.csv")


get_all_player_stats<-function(to_scrape="totals"){
  a<-tibble()
  if (to_scrape %in% c("per_poss-team", "per_poss-opponent")){
    a<-get_season_range_player_stats(seas_range = 2020:1974,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_player_stats(seas_range = 1976:1974,league="ABA",to_scrape = to_scrape))
  }
  else if (to_scrape %in% c("shooting", "play-by-play")){
    a<-get_season_range_player_stats(seas_range = 2020:1997,league="NBA",to_scrape = to_scrape)
  }
  else{
    a<-get_season_range_team_stats(seas_range = 2020:1950,league="NBA",to_scrape = to_scrape) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1976:1968,league="ABA",to_scrape = to_scrape)) %>%
      bind_rows(.,get_season_range_team_stats(seas_range = 1950:1947,league="BAA",to_scrape = to_scrape))
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
write_csv(totals_info %>% select(-hof), "Player Season Info.csv")
write_csv(career_info,"Player Career Info.csv")

totals <- get_all_player_stats("totals") %>% select(-hof)

per_game <- get_all_player_stats("per_game") %>% select(-hof)
per_game <- per_game %>% rename_at(vars(-c(1:12, 16, 19, 22:23, 26)), ~ paste0(., "_per_game"))

advanced <- get_all_player_stats("advanced") %>% select(-hof)
advanced <- advanced %>% select(-c(x, x_2))

per_36 <- get_all_player_stats("per_minute") %>% select(-hof)
per_36 <- per_36 %>% rename_at(vars(-c(1:13, 16, 19, 22, 25)), ~ paste0(., "_per_36_min"))

per_100 <- get_all_player_stats("per_poss") %>% select(-hof)
per_100 <- per_100 %>%
  select(-x) %>%
  filter(season > 1973)
per_100 <- per_100 %>% rename_at(vars(-c(1:13, 16, 19, 22, 25, 35:36)), ~ paste0(., "_per_100_poss"))

shooting <- get_all_player_stats("shooting") %>% select(-hof)
shooting <- shooting %>%
  rename(avg_dist_fga = dist) %>%
  rename_at(vars(c(15:20)), ~ paste0("percent_fga_from_", ., "_range")) %>%
  rename_at(vars(c(21:26)), ~ paste0("fg_percent_from_", str_sub(., end = -3), "_range")) %>%
  rename_at(vars(c(27:28)), ~ paste0("percent_assisted_", str_sub(., end = -3), "_fg")) %>%
  rename(
    percent_dunks_of_fga = percent_fga, num_of_dunks = number,
    percent_corner_3s_of_3pa = percent_3pa, corner_3_point_percent = x3p_percent,
    num_heaves_attempted = att, num_heaves_made = number_2
  )

play_by_play <- get_all_player_stats("play-by-play") %>% select(-hof)
play_by_play <- play_by_play %>%
  rename(
    on_court_plus_minus_per_100_poss = on_court,
    net_plus_minus_per_100_poss = on_off, bad_pass_turnover = bad_pass,
    lost_ball_turnover = lost_ball, shooting_foul_committed = shoot,
    offensive_foul_committed = off, shooting_foul_drawn = shoot_2,
    offensive_foul_drawn = off_2, points_generated_by_assists = pga,
    fga_blocked = blkd
  )

write_excel_csv(advanced, "Data/Advanced.csv")
write_excel_csv(per_100, "Data/Per 100 Poss.csv")
write_excel_csv(per_36, "Data/Per 36 Minutes.csv")
write_excel_csv(totals, "Data/Player Totals.csv")
write_excel_csv(per_game, "Data/Player Per Game.csv")
write_excel_csv(play_by_play, "Data/Player Play By Play.csv")
write_excel_csv(shooting, "Data/Player Shooting.csv")
