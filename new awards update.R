library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("award shares.R")

curr_year=2021
mvp <- get_award_pcts_mvp_roy(season = curr_year, award = "mvp")
roy <- get_award_pcts_mvp_roy(season = curr_year, award = "roy")
mip <- get_award_pcts_other(season = curr_year, award = "mip")
dpoy <- get_award_pcts_other(season = curr_year, award = "dpoy")
smoy <- get_award_pcts_other(season = curr_year, award = "smoy")

new_seas_awards <- bind_rows(dpoy, smoy, mip, mvp, roy)

winners <- new_seas_awards %>%
  group_by(award) %>%
  slice_max(share) %>% ungroup()
new_seas_awards <- anti_join(new_seas_awards, winners)
winners <- winners %>% mutate(winner = TRUE)
new_seas_awards <- full_join(new_seas_awards, winners) %>% arrange(award, desc(share))
psi <- read_csv("Player Season Info.csv")
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

old_awards <- read_csv("Player Award Shares.csv")
write_csv(old_awards %>% 
            filter(season != curr_year) %>% 
            add_row(final_new_seas_awards) %>%
            arrange(desc(season),award,desc(share)), "Player Award Shares.csv")

new_seas_allstars <- all_stars(season = curr_year) %>%
  mutate(
    replaced = str_detect(player, "\\("),
    player = ifelse(replaced == TRUE, substr(player, 1, nchar(player) - 4), player),
    hof = str_detect(player, "\\*"),
    player = ifelse(hof == TRUE, substr(player, 1, nchar(player) - 1), player)
  ) %>% select(-hof)

write_csv(read_csv("All-Star Selections.csv") %>% filter(season != curr_year) %>%
            add_row(new_seas_allstars) %>% arrange(desc(season),lg,team), "All-Star Selections.csv")

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
    TRUE~player
  )
  ) %>% left_join(.,psi) %>% select(season:birth_year,tm,age)

write_csv(read_csv("End of Season Teams.csv") %>% 
            filter(season != curr_year) %>%
            add_row(new_end_seas_teams) %>% 
            arrange(desc(season), type, number_tm),
          "End of Season Teams.csv")

all_lg <- all_lg_voting(season=curr_year)
new_all_lg <- left_join(all_lg, psi) %>% select(-c(birth_year:experience))

write_csv(read_csv("End of Season Teams (Voting).csv") %>% 
            filter(season != curr_year) %>%
            add_row(new_all_lg) %>% 
            arrange(desc(season), type, number_tm),
          "End of Season Teams (Voting).csv")
