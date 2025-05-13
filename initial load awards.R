library(rvest)
library(tidyverse)
library(janitor)
library(polite)

source("award shares.R")

current_year=2024

award_seasons=bind_rows(
  tibble(league="nba",award="mvp",season=current_year:1956),
  tibble(league="aba",award="mvp",season=1976:1968),
  tibble(league="nba",award="roy",season=c(current_year:1964,1959)),
  tibble(league="aba",award="roy",season=1975:1972),
  tibble(league="nba",award="dpoy",season=current_year:1983),
  tibble(league="nba",award="smoy",season=current_year:1984),
  tibble(league="nba",award="mip",season=current_year:1986),
  tibble(league="nba",award="clutch_poy",season=current_year:2023)
)

awards<-tibble()
for (row in 1:nrow(award_seasons)) {
  curr_lg=award_seasons$league[row]
  curr_seas=award_seasons$season[row]
  curr_award=award_seasons$award[row]
  awards=bind_rows(
    awards,
    get_award_pcts(lg=curr_lg,season=curr_seas,award=curr_award)
  )
  print(paste(curr_seas,curr_lg,curr_award))
}


session=nod(bbref_bow,path="awards/roy.html") %>% scrape()

# add nba roy winners with no voting results
nba_roys <- session %>%
  html_elements(css = "#div_roy_NBA") %>% 
  html_table() %>% .[[1]]  %>%
  # first row is actual column names
  row_to_names(1) %>% clean_names()

nba_roy_player_ids=tibble(
  player_id=session %>% 
    html_elements(css = "#div_roy_NBA") %>% 
    html_elements("td") %>%
    #data-append-csv have slugs
    html_attr("data-append-csv")) %>% 
  filter(!is.na(player_id))

nba_roys_without_voting <- bind_cols(nba_roys,nba_roy_player_ids) %>%
  filter(voting != "(V)") %>%
  mutate(season = as.numeric(substr(season, 0, 4)) + 1) %>%
  # remove asterisks (unofficial recognize)
  mutate(player = ifelse(str_detect(player, "\\*"), substr(player, 1, nchar(player) - 2), player)) %>%
  # remove trailing tie in 1952
  mutate(player = ifelse(str_detect(player, "(Tie)"), substr(player, 1, nchar(player) - 6), player)) %>%
  select(-c(voting, tm:ws_48)) %>%
  mutate(award = ifelse(lg == "NBA", "nba roy", "baa roy")) %>%
  relocate(award, .before = player) %>%
  select(-lg) %>%
  mutate(winner = TRUE) %>% mutate(age=as.numeric(age))

aba_roys <- session %>%
  html_elements(css = "#div_roy_ABA") %>% 
  html_table() %>% .[[1]]  %>%
  # first row is actual column names
  row_to_names(1) %>% clean_names()

aba_roy_player_ids=tibble(
  player_id=session %>% 
    html_elements(css = "#div_roy_ABA") %>%
    html_elements("td") %>%
    #data-append-csv have slugs
    html_attr("data-append-csv")) %>% 
  filter(!is.na(player_id))

# add aba roy winners with no voting results
aba_roys_without_voting <- bind_cols(aba_roys,aba_roy_player_ids) %>%
  filter(voting != "(V)") %>%
  mutate(season = as.numeric(substr(season, 0, 4)) + 1) %>%
  # 1971 was a tie, so get rid of trailing (Tie) remark for Charlie Scott and Dan Issel
  mutate(player = ifelse(str_detect(player, "(Tie)"), substr(player, 1, nchar(player) - 6), player)) %>%
  select(-c(voting, tm:ws_48)) %>%
  add_column(award = "aba roy", .before = "player") %>%
  select(-lg) %>%
  add_column("winner" = TRUE) %>% mutate(age=as.numeric(age))


final_awards <- bind_rows(awards, nba_roys_without_voting,aba_roys_without_voting) %>%
  # no voting found for 1983 smoy
  add_row(season = 1983, award = "nba smoy", player = "Bobby Jones", age = 31, winner = TRUE) %>%
  arrange(desc(season),award,desc(share))

write_csv(final_awards, "Data/Player Award Shares.csv")

all_lg_voting_seasons=bind_rows(
  tibble(league="nba",award="all_nba",season=current_year:1966),
  tibble(league="nba",award="all_rookie",season=current_year:1963),
  tibble(league="nba",award="all_defense",season=current_year:1969)
  )

all_lg<-tibble()
for (row in 1:nrow(all_lg_voting_seasons)) {
  curr_lg=all_lg_voting_seasons$league[row]
  curr_seas=all_lg_voting_seasons$season[row]
  curr_award=all_lg_voting_seasons$award[row]
  all_lg=bind_rows(
    all_lg,
    all_lg_voting(league=curr_lg,season=curr_seas,award=curr_award)
  )
  print(paste(curr_seas,curr_lg,curr_award))
}

final_all_lg=all_lg %>% arrange(desc(season), type, number_tm)

write_csv(all_lg,"Data/End of Season Teams (Voting).csv")

all_league <- end_seas_team_scrape("all_league")
all_def <- end_seas_team_scrape("all_defense")
all_rook <- end_seas_team_scrape("all_rookie")

end_seas_teams <- bind_rows(all_league, all_def, all_rook)

#for some reason, names on these pages omit suffixes
#jaren jackson instead of jaren jackson jr
#jabari smith instead of jabari smith jr
#join with player career info to get true names

pci=read_csv("Data/Player Career Info.csv") %>% select(player,player_id)

final_end_seas=left_join(end_seas_teams,pci,
                         by=join_by(player_id==player_id),
                         suffix = c("_orig","")) %>%
  select(-player_orig) %>% relocate(player,.before=player_id) %>%
  arrange(desc(season),lg,type)

write_csv(final_end_seas, "Data/End of Season Teams.csv")

all_star_years=bind_rows(
  #no all-stars in 1999
  tibble(league="NBA",season=setdiff(current_year:1951, 1999)),
  tibble(league="ABA",season=1976:1968),
)

all_stars_all_years <- tibble()
for (row in 1:nrow(all_star_years)) {
  curr_lg=all_star_years$league[row]
  curr_seas=all_star_years$season[row]
  all_stars_all_years=bind_rows(
    all_stars_all_years,
    all_stars(league=curr_lg,season=curr_seas)
  )
  print(paste(curr_seas,curr_lg))
}

all_stars_cleaned <- all_stars_all_years %>% select(-hof) %>% arrange(desc(season),lg,team)

write_csv(all_stars_cleaned, "Data/All-Star Selections.csv")
