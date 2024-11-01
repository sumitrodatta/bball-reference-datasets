library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("Tm & Player Seasons.R")

current_seas=2025

add_new_team_seas <- function(seas = 2021, type = "per_game-team", update_abbrevs = FALSE) {
  a <- teamStats(season = seas, type = type)
  a<-a %>% mutate(
    playoffs = str_detect(team, "\\*"),
    team = ifelse(playoffs == TRUE, substr(team, 1, nchar(team) - 1), team)
  )
  if (update_abbrevs == TRUE) {
    abbrev <- read_csv("Data/Team Abbrev.csv") %>% filter(season != seas)
    new_seas <- a %>%
      select(season:team,playoffs) %>% filter(team != "League Average")
    previous_seas <- abbrev %>%
      filter(season == seas - 1) %>%
      select(team, abbreviation)
    new_seas <- left_join(new_seas, previous_seas) %>% arrange(team)
    abbrev <- abbrev %>% add_row(new_seas) %>% arrange(desc(season),team)
    write_csv(abbrev, "Data/Team Abbrev.csv")
  }
  a <- left_join(a, read_csv("Data/Team Abbrev.csv")) %>%
    relocate(c(abbreviation, playoffs), .after = "team")
  if (type == "advanced-team") {
    old <- read_csv("Data/Team Summaries.csv") %>% filter(season != seas)
    a <- a %>%
      mutate(attend = gsub(",", "", attend), attend_g = gsub(",", "", attend_g)) %>%
      mutate(across(c(attend, attend_g), as.numeric))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Data/Team Summaries.csv")
  }
  else if (type == "totals-team") {
    old <- read_csv("Data/Team Totals.csv") %>% filter(season != seas)
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Data/Team Totals.csv")
  }
  else if (type == "totals-opponent") {
    old <- read_csv("Data/Opponent Totals.csv") %>% filter(season != seas)
    a <- a %>% rename_at(vars(-c(1:7)), ~ paste0("opp_", .))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Data/Opponent Totals.csv")
  }
  else if (type == "per_game-team") {
    old <- read_csv("Data/Team Stats Per Game.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Data/Team Stats Per Game.csv")
  }
  else if (type == "per_game-opponent") {
    old <- read_csv("Data/Opponent Stats Per Game.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
      rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Data/Opponent Stats Per Game.csv")
  }
  else if (type == "per_poss-team") {
    old <- read_csv("Data/Team Stats Per 100 Poss.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Data/Team Stats Per 100 Poss.csv")
  }
  else if (type == "per_poss-opponent") {
    old <- read_csv("Data/Opponent Stats Per 100 Poss.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
      rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Data/Opponent Stats Per 100 Poss.csv")
  }
}

add_new_seas <- function(seas = 2021, type = "totals", update_psi = FALSE) {
  a <- scrape_stats(season = seas, type = type)
  # no need for alt names after full data reload
  # alt_names=read_csv("Data/Alternate Player Names.csv")
  # a=left_join(a,alt_names) %>% 
  #   mutate(player=coalesce(player_alternate,player)) %>% select(-player_alternate)
  if (update_psi == TRUE) {
    new_player_info <- a %>%
      select(season:tm) %>%
      arrange(season, player) %>% 
      mutate(birth_year=case_when((player=="Mike James" & season>=2018)~1990,
                                  (player=="George King" & season>=2019)~1994,
                                  (player=="Brandon Williams" & season>=2022)~1999,
                                  (player=="Johnny Davis" & season>=2023)~2002,
                                  TRUE~NA_real_))
    # change above mutate birth year line whenever new player enters league
    psi <- read_csv("Data/Player Season Info.csv") %>%
      select(season, player:tm) %>%
      filter(season != seas) %>%
      arrange(season, player)
    pci <- read_csv("Data/Player Career Info.csv") %>% filter(last_seas<seas) %>%
      select(player_id:hof)
    updated_psi <- psi %>% add_row(new_player_info)
    # season ids
    updated_psi <- updated_psi %>% mutate(seas_id = row_number())
    players_unique <- updated_psi %>%
      group_by(player, birth_year) %>%
      slice(1) %>%
      arrange(seas_id) %>%
      ungroup() %>%
      mutate(player_id = row_number()) %>%
      select(player, birth_year, player_id)
    updated_psi <- left_join(updated_psi, players_unique)
    # add years of experience
    updated_psi <- updated_psi %>% mutate(tm = ifelse(tm == "TOT", "1TOT", tm))
    removed_mult_same_yr <- updated_psi %>%
      group_by(player_id, season) %>%
      arrange(tm) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(player_id) %>%
      mutate(experience = row_number())
    updated_psi <- left_join(updated_psi, removed_mult_same_yr) %>%
      arrange(player_id, season, tm) %>%
      fill(experience) %>%
      mutate(tm = ifelse(tm == "1TOT", "TOT", tm)) %>%
      arrange(seas_id) %>%
      relocate(seas_id, player_id, .after = "season")
    updated_pci <- updated_psi %>% group_by(player_id,player,birth_year) %>% 
      summarize(num_seasons=max(experience),first_seas=min(season),
                last_seas=max(season)) %>% 
      ungroup() %>% group_by(player_id) %>% slice_head(n=1) %>% ungroup() %>%
      full_join(pci,.) %>% replace_na(list(hof=FALSE)) %>% arrange(player_id)
    write_csv(updated_pci, "Data/Player Career Info.csv")
    write_csv(updated_psi, "Data/Player Season Info.csv")
  }
  # positions in upgraded tables truncated
  if (type %in% c("advanced","per_poss","shooting","play_by_play")){
    a <- a %>% select(-pos)
  }
  a <- left_join(a, read_csv("Data/Player Season Info.csv",col_types = "dddcdcdccd")) %>%
    relocate(seas_id, season, player_id, player, birth_year, pos, age, experience, lg)
  if (type == "totals") {
    old <- read_csv("Data/Player Totals.csv") %>% filter(season != seas)
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Data/Player Totals.csv")
  }
  else if (type == "advanced") {
    old <- read_csv("Data/Advanced.csv") %>% filter(season != seas)
    a <- a %>% select(-c(x, x_2))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Data/Advanced.csv")
  }
  else if (type == "per_game") {
    old <- read_csv("Data/Player Per Game.csv") %>% filter(season != seas)
    a <- a %>% rename_at(vars(-c(1:12, 16, 19, 22:23, 26)), ~ paste0(., "_per_game"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Data/Player Per Game.csv")
  }
  else if (type == "per_minute") {
    old <- read_csv("Data/Per 36 Minutes.csv") %>% filter(season != seas)
    a <- a %>% rename_at(vars(-c(1:13, 16, 19, 22, 25)), ~ paste0(., "_per_36_min"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Data/Per 36 Minutes.csv")
  }
  else if (type == "per_poss") {
    old <- read_csv("Data/Per 100 Poss.csv") %>% filter(season != seas)
    a <- a %>%
      select(-x) %>%
      rename_at(vars(-c(1:13, 16, 19, 22, 25, 35:36)), ~ paste0(., "_per_100_poss"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Data/Per 100 Poss.csv")
  }
  else if (type == "shooting") {
    old <- read_csv("Data/Player Shooting.csv") %>% filter(season != seas)
    a <- a %>%
      rename(avg_dist_fga = dist) %>%
      rename_at(vars(c(15:20)), ~ paste0("percent_fga_from_", ., "_range")) %>%
      rename_at(vars(c(21:26)), ~ paste0("fg_percent_from_", str_sub(., end = -3), "_range")) %>%
      rename_at(vars(c(27:28)), ~ paste0("percent_assisted_", str_sub(., end = -3), "_fg")) %>%
      rename(
        percent_dunks_of_fga = percent_fga, num_of_dunks = number,
        percent_corner_3s_of_3pa = percent_3pa, corner_3_point_percent = x3p_percent,
        num_heaves_attempted = att, num_heaves_made = number_2
      )
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Data/Player Shooting.csv")
  }
  else if (type == "play-by-play") {
    old <- read_csv("Data/Player Play By Play.csv") %>% filter(season != seas)
    a <- a %>% rename(
      on_court_plus_minus_per_100_poss = on_court,
      net_plus_minus_per_100_poss = on_off, bad_pass_turnover = bad_pass,
      lost_ball_turnover = lost_ball, shooting_foul_committed = shoot,
      offensive_foul_committed = off, shooting_foul_drawn = shoot_2,
      offensive_foul_drawn = off_2, points_generated_by_assists = pga,
      fga_blocked = blkd
    )
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Data/Player Play By Play.csv")
  }
}

add_new_team_seas(seas = current_seas, type = "totals-team", update_abbrevs = TRUE)
add_new_team_seas(seas = current_seas, type = "totals-opponent", update_abbrevs = FALSE)
add_new_team_seas(seas = current_seas, type = "per_game-team", update_abbrevs = FALSE)
add_new_team_seas(seas = current_seas, type = "per_game-opponent", update_abbrevs = FALSE)
add_new_team_seas(seas = current_seas, type = "per_poss-team", update_abbrevs = FALSE)
add_new_team_seas(seas = current_seas, type = "per_poss-opponent", update_abbrevs = FALSE)
add_new_team_seas(seas = current_seas, type = "advanced-team", update_abbrevs = FALSE)

add_new_seas(seas = current_seas, type = "totals", update_psi = TRUE)
add_new_seas(seas = current_seas, type = "advanced", update_psi = FALSE)
add_new_seas(seas = current_seas, type = "per_game", update_psi = FALSE)
add_new_seas(seas = current_seas, type = "per_minute", update_psi = FALSE)
add_new_seas(seas = current_seas, type = "per_poss", update_psi = FALSE)
add_new_seas(seas = current_seas, type = "shooting", update_psi = FALSE)
add_new_seas(seas = current_seas, type = "play-by-play", update_psi = FALSE)

curr_rookies=get_rookies(current_seas)

rookies_from_pci=read_csv("Data/Player Career Info.csv") %>% filter(first_seas==current_seas)

full_join(curr_rookies,rookies_from_pci) %>% filter(is.na(debut)|is.na(player_id))

# running this removes birth years for old players now
# file_names=c("Data/Player Totals.csv","Data/Advanced.csv","Data/Player Per Game.csv",
#              "Data/Per 36 Minutes.csv","Data/Per 100 Poss.csv","Data/Player Shooting.csv","Data/Player Play By Play.csv")
# 
# sapply(file_names,function(x){
#   write_csv(read_csv(x) %>% select(-birth_year) %>% left_join(.,read_csv("Data/Player Season Info.csv")) %>% 
#               relocate(birth_year,.after=player),x)
# })
