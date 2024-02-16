library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("award shares.R")

psi <- read_csv("Data/Player Season Info.csv")
curr_year=2024

mvp <- get_award_pcts_mvp_roy(season = curr_year, award = "mvp")
roy <- get_award_pcts_mvp_roy(season = curr_year, award = "roy")
mip <- get_award_pcts_other(season = curr_year, award = "mip")
dpoy <- get_award_pcts_other(season = curr_year, award = "dpoy")
smoy <- get_award_pcts_other(season = curr_year, award = "smoy")

new_seas_awards <- bind_rows(dpoy, smoy, mip, mvp, roy) %>%
  arrange(award,desc(share))

final_new_seas_awards <- new_seas_awards %>%
  mutate(player=case_when(
    (player == "Jaren Jackson" & season >=2019)~"Jaren Jackson Jr.",
    (player == "Marvin Bagley" & season >= 2019)~"Marvin Bagley III",
    (player == "Dennis Smith" & season >= 2018)~"Dennis Smith Jr.",
    (player == "Taurean Waller-Prince" & season >= 2018)~"Taurean Prince",
    (player == "Tim Hardaway" & season >= 2014)~"Tim Hardaway Jr.",
    (player == "Nenê Hilário" & season >= 2003)~"Nenê",
    (player == "Michael Porter" & season >= 2020)~"Michael Porter Jr.",
    TRUE~player
  )
  ) %>%
  left_join(., psi) %>%
  select(-c(birth_year:experience))

old_awards <- read_csv("Data/Player Award Shares.csv")
write_csv(old_awards %>% 
            filter(season != curr_year) %>% 
            add_row(final_new_seas_awards) %>%
            arrange(desc(season),award,desc(share)), "Data/Player Award Shares.csv")

new_seas_allstars <- all_stars(season = curr_year) %>%
  mutate(
    replaced = str_detect(player, "\\("),
    player = ifelse(replaced == TRUE, substr(player, 1, nchar(player) - 4), player),
    hof = str_detect(player, "\\*"),
    player = ifelse(hof == TRUE, substr(player, 1, nchar(player) - 1), player)
  ) %>% select(-hof)

write_csv(read_csv("Data/All-Star Selections.csv") %>% filter(season != curr_year) %>%
            add_row(new_seas_allstars) %>% arrange(desc(season),lg,team), "Data/All-Star Selections.csv")

all_lg_without_voting <- all_lg_scrape()
alldef <- all_def_or_all_rookie()
allrook <- all_def_or_all_rookie("all_rookie")

new_end_seas_teams=bind_rows(all_lg_without_voting,alldef,allrook) %>% filter(season==curr_year) %>%
  mutate(player=case_when(
    (player == "Jaren Jackson" & season >=2019)~"Jaren Jackson Jr.",
    (player == "Marvin Bagley" & season >= 2019)~"Marvin Bagley III",
    (player == "Dennis Smith" & season >= 2018)~"Dennis Smith Jr.",
    (player == "Taurean Waller-Prince" & season >= 2018)~"Taurean Prince",
    (player == "Tim Hardaway" & season >= 2014)~"Tim Hardaway Jr.",
    (player == "Nenê Hilário" & season >= 2003)~"Nenê",
    (player == "Michael Porter" & season >= 2020)~"Michael Porter Jr.",
    (player == "Jabari Smith" & season>=2023)~"Jabari Smith Jr.",
    TRUE~player
  )
  ) %>% left_join(.,psi) %>% select(season:birth_year,tm,age)

write_csv(read_csv("Data/End of Season Teams.csv") %>% 
            filter(season != curr_year) %>%
            add_row(new_end_seas_teams) %>% 
            arrange(desc(season), type, number_tm),
          "Data/End of Season Teams.csv")

all_lg <- all_lg_voting(season=curr_year,award="all_nba")
all_def_voting <- all_lg_voting(season=curr_year,award="all_defense")
new_all_lg <- bind_rows(all_lg,all_def_voting) %>% left_join(., psi) %>% select(-c(birth_year:experience))

write_csv(read_csv("Data/End of Season Teams (Voting).csv") %>% 
            filter(season != curr_year) %>%
            add_row(new_all_lg) %>% 
            arrange(desc(season), type, number_tm),
          "Data/End of Season Teams (Voting).csv")
