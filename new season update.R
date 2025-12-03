library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("Tm & Player Seasons.R")

current_seas=2026

# #run this once during first update of season (draft info wouldn't change on month by month basis)
# old_dph=read_csv("Data/Draft Pick History.csv") %>% filter(season != current_seas-1)
# new_seas_draft_picks=get_draft_picks(current_seas-1)
# new_dph=bind_rows(old_dph,new_seas_draft_picks) %>%
#   arrange(desc(season),lg,overall_pick)
# write_csv(new_dph,"Data/Draft Pick History.csv")

add_new_team_seas <- function(seas = 2021, type = "per_game-team") {
  file_stat_relations=tribble(
    ~stat_type,~file,
    "totals-team","Team Totals",
    "totals-opponent","Opponent Totals",
    "per_game-team","Team Stats Per Game",
    "per_game-opponent","Opponent Stats Per Game",
    "per_poss-team","Team Stats Per 100 Poss",
    "per_poss-opponent","Opponent Stats Per 100 Poss",
    "advanced-team","Team Summaries"
  )
  a <- team_stats(season = seas, type = type)
  
  filename_to_update=paste0("Data/",file_stat_relations %>% filter(stat_type==type) %>% pull(file),".csv")
  
  file_to_update <- read_csv(filename_to_update)
  
  updated_file <- file_to_update %>% filter(season != seas) %>%
    bind_rows(a) %>% arrange(desc(season),lg,abbreviation)
  
  write_csv(updated_file,filename_to_update)
}

add_new_team_seas(seas = current_seas, type = "totals-team")
add_new_team_seas(seas = current_seas, type = "totals-opponent")
add_new_team_seas(seas = current_seas, type = "per_game-team")
add_new_team_seas(seas = current_seas, type = "per_game-opponent")
add_new_team_seas(seas = current_seas, type = "per_poss-team")
add_new_team_seas(seas = current_seas, type = "per_poss-opponent")
add_new_team_seas(seas = current_seas, type = "advanced-team")

team_abbrevs=read_csv("Data/Team Totals.csv") %>% select(season:playoffs) %>% filter(team !="League Average")
write_csv(team_abbrevs,"Data/Team Abbrev.csv")


add_new_seas <- function(seas = 2021, type = "totals") {
  file_stat_relations=tribble(
    ~stat_type,~file,
    "totals","Player Totals",
    "per_game","Player Per Game",
    "per_minute","Per 36 Minutes",
    "per_poss","Per 100 Poss",
    "advanced","Advanced",
    "shooting","Player Shooting",
    "play-by-play","Player Play By Play"
  )
  a <- player_stats(season = seas, type = type)
  
  filename_to_update=paste0("Data/",file_stat_relations %>% filter(stat_type==type) %>% pull(file),".csv")
  
  file_to_update <- read_csv(filename_to_update)
  
  updated_file <- file_to_update %>% filter(season != seas) %>%
    bind_rows(a) %>% arrange(desc(season),player_id)
  
  write_csv(updated_file,filename_to_update)
}

add_new_seas(seas = current_seas, type = "totals")
add_new_seas(seas = current_seas, type = "advanced")
add_new_seas(seas = current_seas, type = "per_game")
add_new_seas(seas = current_seas, type = "per_minute")
add_new_seas(seas = current_seas, type = "per_poss")
add_new_seas(seas = current_seas, type = "shooting")
add_new_seas(seas = current_seas, type = "play-by-play")

# add experience variable
totals_info <- read_csv("Data/Player Totals.csv") %>%
  select(season:pos)
removed_mult_same_yr <- totals_info %>%
  arrange(season, player_id) %>%
  #for those who played for N teams in season, have N+1 rows
  #first row has team listed as [N]TM
  group_by(player_id, season) %>%
  slice_min(team) %>%
  ungroup() %>%
  group_by(player_id) %>%
  mutate(experience = row_number()) %>%
  ungroup()
final_totals_info <- left_join(totals_info, removed_mult_same_yr) %>%
  arrange(player_id, season, team) %>%
  #fill same experience for same season down for multiple-team players
  fill(experience) %>%
  arrange(season,player_id,team)

write_csv(final_totals_info, "Data/Player Season Info.csv")

player_directory=get_player_directory()

pci=read_csv("Data/Player Career Info.csv")

players_to_update=anti_join(pci,player_directory)
#multiple types of updates
#new season means to is updated
#measurements could change position, height, weight
#rookies will not have debuts

no_updates_needed=inner_join(pci,player_directory)

updated_players=left_join(players_to_update,player_directory,
          by=join_by(player_id==player_id),
          #old values from players_to_update->pci
          suffix = c("_old","")) %>% 
  select(-ends_with("_old"))

updated_no_rookies=bind_rows(no_updates_needed,updated_players)

#players not in pci -> must be rookies
curr_rookies=get_rookie_debuts(season=current_seas) %>% select(-c(season,league))

#only get rookies that have debuted since last update
#if they were already on pci, would have been updated in players_to_update
rookies_not_in_pci=anti_join(curr_rookies,updated_no_rookies)

rookies_w_debuts=left_join(rookies_not_in_pci,player_directory) %>% 
  replace_na(list(from=current_seas,to=current_seas,hof=FALSE))

updated_full=bind_rows(updated_no_rookies,rookies_w_debuts) %>% arrange(from,debut,player_id)

write_csv(updated_full,"Data/Player Career Info.csv")
