library(rvest)
library(tidyverse)
library(janitor)

source("Tm & Player Seasons.R")

team_stats_base <- get_all_team_stats()

opp_stats_base <- get_all_team_stats("opponent-stats-base")
opp_stats_base <- opp_stats_base %>% rename_at(vars(-c(1:7)), ~ paste0("opp_", .))

team_stats_pg <- get_all_team_stats("team-stats-per_game")
team_stats_pg <- team_stats_pg %>%
  rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))

opp_stats_pg <- get_all_team_stats("opponent-stats-per_game")
opp_stats_pg <- opp_stats_pg %>%
  rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
  rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))

team_stats_per_poss <- get_all_team_stats("team-stats-per_poss")
team_stats_per_poss <- team_stats_per_poss %>%
  rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))

opp_stats_per_poss <- get_all_team_stats("opponent-stats-per_poss")
opp_stats_per_poss <- opp_stats_per_poss %>%
  rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
  rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))

tm_summaries <- misc_stats_scrape()
sapply(2019:1950, function(x) {
  new_seas <- misc_stats_scrape(x)
  tm_summaries <<- rbind(tm_summaries, new_seas)
})
sapply(1949:1947, function(x) {
  new_seas <- misc_stats_scrape(x, "BAA")
  tm_summaries <<- rbind(tm_summaries, new_seas)
})
sapply(1976:1968, function(x) {
  new_seas <- misc_stats_scrape(x, "ABA")
  tm_summaries <<- rbind(tm_summaries, new_seas)
})
tm_summaries <- left_join(tm_summaries, read_csv("Team Abbrev.csv")) %>%
  relocate(c(abbreviation, playoffs), .after = "team")

write_excel_csv(tm_summaries, "Team Summaries.csv")
write_excel_csv(opp_stats_per_poss, "Opponent Stats Per 100 Poss.csv")
write_excel_csv(team_stats_per_poss, "Team Stats Per 100 Poss.csv")
write_excel_csv(team_stats_base, "Team Totals.csv")
write_excel_csv(opp_stats_base, "Opponent Totals.csv")
write_excel_csv(team_stats_pg, "Team Stats Per Game.csv")
write_excel_csv(opp_stats_pg, "Opponent Stats Per Game.csv")

totals <- get_all()

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

totals <- get_all() %>% select(-hof)

per_game <- get_all("per_game") %>% select(-hof)
per_game <- per_game %>% rename_at(vars(-c(1:13, 17, 20, 23:24, 27)), ~ paste0(., "_per_game"))

advanced <- get_all("advanced") %>% select(-hof)
advanced <- advanced %>% select(-c(x, x_2))

per_36 <- get_all("per_minute") %>% select(-hof)
per_36 <- per_36 %>% rename_at(vars(-c(1:14, 17, 20, 23, 26)), ~ paste0(., "_per_36_min"))

per_100 <- get_all("per_poss") %>% select(-hof)
per_100 <- per_100 %>%
  select(-x) %>%
  filter(season > 1973)
per_100 <- per_100 %>% rename_at(vars(-c(1:14, 17, 20, 23, 26, 36:37)), ~ paste0(., "_per_100_poss"))

shooting <- get_all("shooting") %>% select(-hof)
shooting <- shooting %>%
  rename(avg_dist_fga = dist) %>%
  rename_at(vars(c(16:21)), ~ paste0("percent_fga_from_", ., "_range")) %>%
  rename_at(vars(c(22:27)), ~ paste0("fg_percent_from_", str_sub(., end = -3), "_range")) %>%
  rename_at(vars(c(28:29)), ~ paste0("percent_assisted_", str_sub(., end = -3), "_fg")) %>%
  rename(
    percent_dunks_of_fga = percent_fga, num_of_dunks = number,
    percent_corner_3s_of_3pa = percent_3pa, corner_3_point_percent = x3p_percent,
    num_heaves_attempted = att, num_heaves_made = number_2
  )

play_by_play <- get_all("play-by-play") %>% select(-hof)
play_by_play <- play_by_play %>%
  rename(
    on_court_plus_minus_per_100_poss = on_court,
    net_plus_minus_per_100_poss = on_off, bad_pass_turnover = bad_pass,
    lost_ball_turnover = lost_ball, shooting_foul_committed = shoot,
    offensive_foul_committed = off, shooting_foul_drawn = shoot_2,
    offensive_foul_drawn = off_2, points_generated_by_assists = pga,
    fga_blocked = blkd
  )

write_excel_csv(advanced, "Advanced.csv")
write_excel_csv(per_100, "Per 100 Poss.csv")
write_excel_csv(per_36, "Per 36 Minutes.csv")
write_excel_csv(totals, "Player Totals.csv")
write_excel_csv(per_game, "Player Per Game.csv")
write_excel_csv(play_by_play, "Player Play By Play.csv")
write_excel_csv(shooting, "Player Shooting.csv")
