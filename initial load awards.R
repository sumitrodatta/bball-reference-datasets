library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("award shares.R")

psi <- read_csv("Player Season Info.csv")

# add nba mvps
mvp <- get_award_pcts_mvp_roy()
sapply(2019:1956, function(x) {
  new_seas <- get_award_pcts_mvp_roy(x, "mvp")
  mvp <<- rbind(mvp, new_seas)
  print(x)
})

# add aba mvps
sapply(1976:1968, function(x) {
  new_seas <- get_award_pcts_mvp_roy(x, "mvp", "aba")
  mvp <<- rbind(mvp, new_seas)
  print(x)
})

# add nba roys
roy <- get_award_pcts_mvp_roy(award = "roy")
sapply(c(2019:1964, 1959), function(x) {
  new_seas <- get_award_pcts_mvp_roy(x, "roy")
  roy <<- rbind(roy, new_seas)
  print(x)
})

# add aba roys
sapply(1975:1972, function(x) {
  new_seas <- get_award_pcts_mvp_roy(x, "roy", "aba")
  roy <<- rbind(roy, new_seas)
  print(x)
})

# add nba roy winners with no voting results
session=nod(bbref_bow,path="awards/roy.html")

nba_roys_without_voting <- scrape(session) %>%
  html_nodes(css = "#div_roy_NBA") %>% 
  html_table() %>% .[[1]]  %>%
  # first row is actual column names
  row_to_names(1) %>% clean_names() %>%
  filter(voting != "(V)") %>%
  mutate(season = as.numeric(substr(season, 0, 4)) + 1) %>%
  # remove asterisks (unofficial recognize)
  mutate(player = ifelse(str_detect(player, "\\*"), substr(player, 1, nchar(player) - 2), player)) %>%
  # remove trailing tie in 1952
  mutate(player = ifelse(str_detect(player, "(Tie)"), substr(player, 1, nchar(player) - 6), player)) %>%
  select(-c(voting, g:ws_48)) %>%
  mutate(award = ifelse(lg == "NBA", "nba roy", "baa roy")) %>%
  relocate(award, .before = player) %>%
  select(-lg) %>%
  add_column("winner" = TRUE) %>% mutate(age=as.numeric(age))

# add aba roy winners with no voting results
aba_roys_without_voting <- scrape(session) %>%
  html_nodes(css = "#div_roy_ABA") %>% 
  html_table() %>% .[[1]]  %>%
  # first row is actual column names
  row_to_names(1) %>% clean_names() %>%
  filter(voting != "(V)") %>%
  mutate(season = as.numeric(substr(season, 0, 4)) + 1) %>%
  # 1971 was a tie, so get rid of trailing (Tie) remark for Charlie Scott and Dan Issel
  mutate(player = ifelse(str_detect(player, "(Tie)"), substr(player, 1, nchar(player) - 6), player)) %>%
  select(-c(voting, g:ws_48)) %>%
  add_column(award = "aba roy", .before = "player") %>%
  select(-lg) %>%
  add_column("winner" = TRUE) %>% mutate(age=as.numeric(age))

final_roys <- bind_rows(roy, nba_roys_without_voting,aba_roys_without_voting)

mip <- get_award_pcts_other(2020, "mip")
seasons <- c(2019:1988, 1986) # no voting found for 1987
sapply(seasons, function(x) {
  new_seas <- get_award_pcts_other(x, "mip")
  mip <<- rbind(mip, new_seas)
  print(x)
})
mip <- mip %>% add_row(season = 1987, award = "mip", player = "Dale Ellis", age = 26, tm = "SEA", winner = TRUE)

dpoy <- get_award_pcts_other(2020, "dpoy")
sapply(2019:1983, function(x) {
  new_seas <- get_award_pcts_other(x, "dpoy")
  dpoy <<- rbind(dpoy, new_seas)
  print(x)
})

smoy <- get_award_pcts_other(2020, "smoy")
seasons <- 2019:1984 # no voting found for 1983
sapply(seasons, function(x) {
  new_seas <- get_award_pcts_other(x, "smoy")
  smoy <<- rbind(smoy, new_seas)
  print(x)
})
smoy <- smoy %>% add_row(season = 1983, award = "smoy", player = "Bobby Jones", age = 31, tm = "PHI", winner = TRUE)

awards <- bind_rows(dpoy, smoy, mip, mvp, roy)

winners <- awards %>%
  group_by(season, award) %>%
  slice_max(share) %>% ungroup()
awards <- anti_join(awards, winners)
winners <- winners %>% mutate(winner = TRUE)
awards <- full_join(awards, winners) %>% arrange(desc(season), award, desc(share))
finalized_awards <- awards %>%
  mutate(player = ifelse(player == "Jaren Jackson" & season >= 2019, "Jaren Jackson Jr.", player)) %>%
  mutate(player = ifelse(player == "Marvin Bagley" & season >= 2019, "Marvin Bagley III", player)) %>%
  mutate(player = ifelse(player == "Dennis Smith" & season >= 2018, "Dennis Smith Jr.", player)) %>%
  mutate(player = ifelse(player == "Taurean Waller-Prince" & season >= 2018, "Taurean Prince", player)) %>%
  mutate(player = ifelse(player == "Tim Hardaway" & season >= 2014, "Tim Hardaway Jr.", player)) %>%
  mutate(player = ifelse(player == "Nenê Hilário" & season >= 2003, "Nenê", player)) %>%
  left_join(., psi) %>%
  select(-c(birth_year:experience))

write_csv(finalized_awards, "Player Award Shares.csv")

all_lg <- all_lg_voting()
sapply(2019:1966, function(x) {
  new_seas <- all_lg_voting(season=x)
  all_lg <<- rbind(all_lg, new_seas)
  print(x)
})

all_lg_without_voting <- all_lg_scrape() %>% left_join(.,psi) %>%
  mutate(tm = ifelse(tm == "TOT", "1TOT", tm)) %>%
  group_by(player_id,season,type) %>%
  arrange(tm) %>%
  slice(1) %>%
  mutate(tm = ifelse(tm == "1TOT", "TOT", tm)) %>% ungroup() %>%
  arrange(desc(season), type, number_tm) %>% select(-c(birth_year:experience))

full_all_lg<-all_lg %>%
  mutate(player = ifelse(player == "Jaren Jackson" & season >= 2019, "Jaren Jackson Jr.", player)) %>%
  mutate(player = ifelse(player == "Marvin Bagley" & season >= 2019, "Marvin Bagley III", player)) %>%
  mutate(player = ifelse(player == "Dennis Smith" & season >= 2018, "Dennis Smith Jr.", player)) %>%
  mutate(player = ifelse(player == "Tim Hardaway" & season >= 2014, "Tim Hardaway Jr.", player)) %>%
  mutate(player = ifelse(player == "Nenê Hilário" & season >= 2003, "Nenê", player)) %>%
  left_join(.,psi) %>%
  mutate(tm = ifelse(tm == "TOT", "1TOT", tm)) %>%
  group_by(player_id, season,type) %>%
  arrange(tm) %>%
  slice(1) %>%
  mutate(tm = ifelse(tm == "1TOT", "TOT", tm)) %>% ungroup() %>%
  arrange(desc(season), type, number_tm) %>% select(-c(birth_year:experience))

final_all_lg=bind_rows(full_all_lg,
                       all_lg_without_voting %>% filter(!(lg=="NBA" & season %in% full_all_lg$season)))

write_csv(final_all_lg,"End of Season Teams (Voting).csv")

alldef <- all_def_or_all_rookie()
allrook <- all_def_or_all_rookie("all_rookie")

end_seas_teams <- bind_rows(all_lg_without_voting, alldef, allrook)

end_seas_teams <- end_seas_teams %>%
  mutate(player = ifelse(player == "Jaren Jackson" & season >= 2019, "Jaren Jackson Jr.", player)) %>%
  mutate(player = ifelse(player == "Marvin Bagley" & season >= 2019, "Marvin Bagley III", player)) %>%
  mutate(player = ifelse(player == "Dennis Smith" & season >= 2018, "Dennis Smith Jr.", player)) %>%
  mutate(player = ifelse(player == "Tim Hardaway" & season >= 2014, "Tim Hardaway Jr.", player)) %>%
  mutate(player = ifelse(player == "Nenê Hilário" & season >= 2003, "Nenê", player))
end_seas_teams <- left_join(end_seas_teams, psi) %>%
  group_by(season, type, player) %>%
  mutate(n = n()) %>%
  select(season:birth_year, tm, age, n)
# two George Johnsons played at same time, one won All-Defense, remove the other
end_seas_teams <- end_seas_teams %>% filter(!(player == "George Johnson" & birth_year == 1956))
# if player played for multiple teams during season, keep only total stat
tots <- end_seas_teams %>% filter(n > 1 & player != "George Johnson")
end_seas_teams <- anti_join(end_seas_teams, tots)
tots <- tots %>%
  mutate(tm = ifelse(tm == "TOT", "1TOT", tm)) %>%
  group_by(player_id, type) %>%
  arrange(tm) %>%
  slice(1) %>%
  mutate(tm = ifelse(tm == "1TOT", "TOT", tm))
end_seas_teams <- full_join(end_seas_teams, tots) %>%
  select(-n) %>%
  arrange(desc(season), type, team_rank)

write_csv(end_seas_teams, "End of Season Teams.csv")

all_stars_all_years <- all_stars(2020)
# nba (no game in 1999)
sapply(setdiff(2019:1951, 1999), function(x) {
  new_seas <- all_stars(x)
  all_stars_all_years <<- rbind(all_stars_all_years, new_seas)
  print(x)
})
# aba
sapply(1976:1968, function(x) {
  new_seas <- all_stars(x, "ABA")
  all_stars_all_years <<- rbind(all_stars_all_years, new_seas)
  print(x)
})

all_stars_cleaned <- all_stars_all_years %>%
  mutate(
    replaced = str_detect(player, "\\("),
    player = ifelse(replaced == TRUE, substr(player, 1, nchar(player) - 4), player),
    hof = str_detect(player, "\\*"),
    player = ifelse(hof == TRUE, substr(player, 1, nchar(player) - 1), player)
  ) %>% select(-hof) %>% arrange(desc(season),lg,team)

write_csv(all_stars_cleaned, "All-Star Selections.csv")
