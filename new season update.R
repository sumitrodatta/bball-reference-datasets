library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("Tm & Player Seasons.R")

add_new_team_seas <- function(seas = 2021, type = "team-stats-base", update_abbrevs = FALSE) {
  if (type == "misc") {
    a <- misc_stats_scrape(season = seas)
  }
  # work around since not updating
  # note as of may 26, 2021: needed when no playoff series in page
  # else if (type=="team-stats-per_game"){
  #   session = nod(bbref_bow,path=paste0("leagues/NBA_", seas, ".html"))
  #   a<-scrape(session) %>% 
  #     html_nodes(xpath = "//comment()") %>% 
  #     html_text() %>% 
  #     paste(collapse = "") %>% 
  #     read_html() %>% 
  #     html_table() %>%
  #     .[[1]] %>%
  #     rename(Season = Rk) %>%
  #     mutate(Season = seas) %>%
  #     add_column(Lg = "NBA", .before = "Team") %>%
  #     clean_names()
  # }
  else {
    a <- teamStats(season = seas, type = type)
  }
  if (update_abbrevs == TRUE) {
    abbrev <- read_csv("Team Abbrev.csv") %>% filter(season != seas)
    new_seas <- a %>%
      select(season:team) %>%
      mutate(
        playoffs = str_detect(team, "\\*"),
        team = ifelse(playoffs == TRUE, substr(team, 1, nchar(team) - 1), team)
      )
    previous_seas <- abbrev %>%
      filter(season == seas - 1) %>%
      select(team, abbreviation)
    new_seas <- left_join(new_seas, previous_seas) %>% arrange(team)
    abbrev <- abbrev %>% add_row(new_seas)
    write_csv(abbrev, "Team Abbrev.csv")
  }
  a <- left_join(a, read_csv("Team Abbrev.csv")) %>%
    relocate(c(abbreviation, playoffs), .after = "team")
  if (type == "misc") {
    old <- read_csv("Team Summaries.csv") %>% filter(season != seas)
    a <- a %>%
      mutate(attend = gsub(",", "", attend), attend_g = gsub(",", "", attend_g)) %>%
      mutate(across(c(attend, attend_g), as.numeric))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Team Summaries.csv")
  }
  else if (type == "team-stats-base") {
    old <- read_csv("Team Totals.csv") %>% filter(season != seas)
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Team Totals.csv")
  }
  else if (type == "opponent-stats-base") {
    old <- read_csv("Opponent Totals.csv") %>% filter(season != seas)
    a <- a %>% rename_at(vars(-c(1:7)), ~ paste0("opp_", .))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Opponent Totals.csv")
  }
  else if (type == "team-stats-per_game") {
    old <- read_csv("Team Stats Per Game.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Team Stats Per Game.csv")
  }
  else if (type == "opponent-stats-per_game") {
    old <- read_csv("Opponent Stats Per Game.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
      rename_at(vars(-c(1:6, 10, 13, 16, 19)), ~ paste0(., "_per_game"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Opponent Stats Per Game.csv")
  }
  else if (type == "team-stats-per_poss") {
    old <- read_csv("Team Stats Per 100 Poss.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Team Stats Per 100 Poss.csv")
  }
  else if (type == "opponent-stats-per_poss") {
    old <- read_csv("Opponent Stats Per 100 Poss.csv") %>% filter(season != seas)
    a <- a %>%
      rename_at(vars(-c(1:7)), ~ paste0("opp_", .)) %>%
      rename_at(vars(-c(1:7, 10, 13, 16, 19)), ~ paste0(., "_per_100_poss"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),abbreviation), "Opponent Stats Per 100 Poss.csv")
  }
}

add_new_seas <- function(seas = 2021, type = "totals", update_psi = FALSE) {
  a <- scrape_stats(season = seas, type = type)
  if (update_psi == TRUE) {
    # no active hall of famers
    new_player_info <- a %>%
      select(season:tm) %>%
      mutate(hof = FALSE) %>%
      arrange(season, player) %>% 
      mutate(birth_year=case_when((player=="Mike James" & season>=2018)~1990,
                                  TRUE~NA_real_))
    # change above mutate birth year line whenever new player enters league
    psi <- read_csv("Player Season Info.csv") %>%
      select(season, player:tm) %>%
      filter(season != seas)
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
    write_csv(updated_psi, "Player Season Info.csv")
  }
  a <- left_join(a, read_csv("Player Season Info.csv",col_types = "dddcdlcdccd")) %>%
    relocate(seas_id, season, player_id, player, birth_year, hof, pos, age, experience, lg)
  if (type == "totals") {
    old <- read_csv("Player Totals.csv") %>% filter(season != seas)
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Player Totals.csv")
  }
  else if (type == "advanced") {
    old <- read_csv("Advanced.csv") %>% filter(season != seas)
    a <- a %>% select(-c(x, x_2))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Advanced.csv")
  }
  else if (type == "per_game") {
    old <- read_csv("Player Per Game.csv") %>% filter(season != seas)
    a <- a %>% rename_at(vars(-c(1:13, 17, 20, 23:24, 27)), ~ paste0(., "_per_game"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Player Per Game.csv")
  }
  else if (type == "per_minute") {
    old <- read_csv("Per 36 Minutes.csv") %>% filter(season != seas)
    a <- a %>% rename_at(vars(-c(1:14, 17, 20, 23, 26)), ~ paste0(., "_per_36_min"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Per 36 Minutes.csv")
  }
  else if (type == "per_poss") {
    old <- read_csv("Per 100 Poss.csv") %>% filter(season != seas)
    a <- a %>%
      select(-x) %>%
      rename_at(vars(-c(1:14, 17, 20, 23, 26, 36:37)), ~ paste0(., "_per_100_poss"))
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Per 100 Poss.csv")
  }
  else if (type == "shooting") {
    old <- read_csv("Player Shooting.csv") %>% filter(season != seas)
    a <- a %>%
      rename(avg_dist_fga = dist) %>%
      rename_at(vars(c(16:21)), ~ paste0("percent_fga_from_", ., "_range")) %>%
      rename_at(vars(c(22:27)), ~ paste0("fg_percent_from_", str_sub(., end = -3), "_range")) %>%
      rename_at(vars(c(28:29)), ~ paste0("percent_assisted_", str_sub(., end = -3), "_fg")) %>%
      rename(
        percent_dunks_of_fga = percent_fga, num_of_dunks = number,
        percent_corner_3s_of_3pa = percent_3pa, corner_3_point_percent = x3p_percent,
        num_heaves_attempted = att, num_heaves_made = number_2
      )
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Player Shooting.csv")
  }
  else if (type == "play-by-play") {
    old <- read_csv("Player Play By Play.csv") %>% filter(season != seas)
    a <- a %>% rename(
      on_court_plus_minus_per_100_poss = on_court,
      net_plus_minus_per_100_poss = on_off, bad_pass_turnover = bad_pass,
      lost_ball_turnover = lost_ball, shooting_foul_committed = shoot,
      offensive_foul_committed = off, shooting_foul_drawn = shoot_2,
      offensive_foul_drawn = off_2, points_generated_by_assists = pga,
      fga_blocked = blkd
    )
    write_csv(old %>% add_row(a) %>% arrange(desc(season),player), "Player Play By Play.csv")
  }
}

add_new_team_seas(seas = 2021, type = "team-stats-base", update_abbrevs = TRUE)
add_new_team_seas(seas = 2021, type = "opponent-stats-base", update_abbrevs = FALSE)
add_new_team_seas(seas = 2021, type = "team-stats-per_game", update_abbrevs = FALSE)
add_new_team_seas(seas = 2021, type = "opponent-stats-per_game", update_abbrevs = FALSE)
add_new_team_seas(seas = 2021, type = "team-stats-per_poss", update_abbrevs = FALSE)
add_new_team_seas(seas = 2021, type = "opponent-stats-per_poss", update_abbrevs = FALSE)
add_new_team_seas(seas = 2021, type = "misc", update_abbrevs = FALSE)

add_new_seas(seas = 2021, type = "totals", update_psi = TRUE)
add_new_seas(seas = 2021, type = "advanced", update_psi = FALSE)
add_new_seas(seas = 2021, type = "per_game", update_psi = FALSE)
add_new_seas(seas = 2021, type = "per_minute", update_psi = FALSE)
add_new_seas(seas = 2021, type = "per_poss", update_psi = FALSE)
add_new_seas(seas = 2021, type = "shooting", update_psi = FALSE)
add_new_seas(seas = 2021, type = "play-by-play", update_psi = FALSE)

file_names=c("Player Totals.csv","Advanced.csv","Player Per Game.csv",
             "Per 36 Minutes.csv","Per 100 Poss.csv","Player Shooting.csv","Player Play By Play.csv")

sapply(file_names,function(x){
  write_csv(read_csv(x) %>% select(-birth_year) %>% left_join(.,read_csv("Player Season Info.csv")) %>% 
              relocate(birth_year,.after=player),x)
})
