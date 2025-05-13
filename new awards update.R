library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("award shares.R")

curr_year=2025

awards=c("mvp","roy","mip","dpoy","smoy","clutch_poy")

new_seas_awards<-tibble()
for (i in 1:length(awards)) {
  curr_award=awards[i]
  new_seas_awards=bind_rows(
    new_seas_awards,
    get_award_pcts(season=curr_year,award=curr_award)
  )
  print(paste(curr_year,curr_award))
}

final_new_seas_awards <- new_seas_awards %>%
  arrange(award,desc(share))

old_awards <- read_csv("Data/Player Award Shares.csv")

updated_awards=old_awards %>% 
  filter(season != curr_year) %>% 
  bind_rows(final_new_seas_awards) %>%
  arrange(desc(season),award,desc(share))

write_csv(updated_awards, "Data/Player Award Shares.csv")

all_league <- end_seas_team_scrape("all_league")
all_def <- end_seas_team_scrape("all_defense")
all_rook <- end_seas_team_scrape("all_rookie")

new_end_seas_teams <- bind_rows(all_league, all_def, all_rook) %>% filter(season==curr_year)

pci=read_csv("Data/Player Career Info.csv") %>% select(player,player_id)

final_new_end_seas=left_join(new_end_seas_teams,pci,
                         by=join_by(player_id==player_id),
                         suffix = c("_orig","")) %>%
  select(-player_orig) %>% relocate(player,.before=player_id) %>%
  arrange(type)

old_end_seas_teams <- read_csv("Data/End of Season Teams.csv")

updated_end_seas_teams = old_end_seas_teams %>% 
  filter(season != curr_year) %>%
  bind_rows(final_new_end_seas) %>% 
  arrange(desc(season), type, number_tm)

write_csv(updated_end_seas_teams,"Data/End of Season Teams.csv")

end_seas_team_types=c("all_nba","all_defense","all_rookie")

new_end_seas_voting<-tibble()
for (i in 1:length(end_seas_team_types)) {
  curr_end_seas_team_type=end_seas_team_types[i]
  new_end_seas_voting=bind_rows(
    new_end_seas_voting,
    all_lg_voting(season=curr_year,award=curr_end_seas_team_type)
  )
  print(paste(curr_year,curr_end_seas_team_type))
}

old_end_seas_voting=read_csv("Data/End of Season Teams (Voting).csv")

updated_end_seas_voting=old_end_seas_voting %>% 
  filter(season != curr_year) %>%
  add_row(new_end_seas_voting) %>% 
  arrange(desc(season), type, number_tm)

write_csv(updated_end_seas_voting,"Data/End of Season Teams (Voting).csv")

new_seas_allstars <- all_stars(season = curr_year) %>% select(-hof)

old_allstars <- read_csv("Data/All-Star Selections.csv")

updated_allstars=old_allstars %>% 
  filter(season != curr_year) %>%
  bind_rows(new_seas_allstars) %>% 
  arrange(desc(season),lg,team)

write_csv(updated_allstars, "Data/All-Star Selections.csv")

