library(rvest)
library(tidyverse)
library(janitor)

source("Tm & Player Seasons.R")

current_year=2025

league_seasons=bind_rows(tibble(league="NBA",season=current_year:1950),
                         tibble(league="ABA",season=1976:1968),
                         tibble(league="BAA",season=1949:1947))

get_all_team_stats<-function(to_scrape="per_game-team"){
  a<-tibble()
  if (to_scrape %in% c("per_poss-team", "per_poss-opponent")){
    seas_to_scrape=league_seasons %>% filter(season>=1974)
  }
  else{
    seas_to_scrape=league_seasons
  }
  for (row in 1:nrow(seas_to_scrape)) {
    curr_lg=seas_to_scrape$league[row]
    curr_seas=seas_to_scrape$season[row]
    a=bind_rows(
      a,
      team_stats(league=curr_lg,season=curr_seas,type = to_scrape)
    )
    print(paste(curr_lg,curr_seas))
  }
  a <- a %>% arrange(desc(season),lg,abbreviation)
  return(a)
}


team_stats_base <- get_all_team_stats("totals-team")

opp_stats_base <- get_all_team_stats("totals-opponent")

team_stats_pg <- get_all_team_stats("per_game-team")

opp_stats_pg <- get_all_team_stats("per_game-opponent")

team_stats_per_poss <- get_all_team_stats("per_poss-team")

opp_stats_per_poss <- get_all_team_stats("per_poss-opponent")

tm_summaries <- get_all_team_stats("advanced-team")

write_csv(tm_summaries, "Data/Team Summaries.csv")
write_csv(opp_stats_per_poss, "Data/Opponent Stats Per 100 Poss.csv")
write_csv(team_stats_per_poss, "Data/Team Stats Per 100 Poss.csv")
write_csv(team_stats_base, "Data/Team Totals.csv")
write_csv(opp_stats_base, "Data/Opponent Totals.csv")
write_csv(team_stats_pg, "Data/Team Stats Per Game.csv")
write_csv(opp_stats_pg, "Data/Opponent Stats Per Game.csv")

team_abbrevs=team_stats_base %>% select(season:playoffs) %>% filter(team !="League Average")
write_csv(team_abbrevs,"Data/Team Abbrev.csv")


player_directory=get_player_directory()

rookie_debuts=tibble()
for (row in 1:nrow(league_seasons)) {
  curr_lg=league_seasons$league[row]
  curr_seas=league_seasons$season[row]
  rookie_debuts=bind_rows(
    rookie_debuts,
    get_rookie_debuts(league=curr_lg,
                      season=curr_seas)
  )
  print(paste(curr_lg,curr_seas))
}

clean_debuts=rookie_debuts %>%
  group_by(player_id) %>% slice_min(debut) %>% ungroup()

player_career_info=left_join(player_directory,
                             clean_debuts %>% select(player:player_id)) %>%
  arrange(from,debut,player_id,birth_date) %>%
  relocate(player,player_id) %>%
  relocate(from,to,debut,hof,.after=everything())

get_all_player_stats<-function(to_scrape="totals"){
  a<-tibble()
  if (to_scrape == "per_poss"){
    seas_to_scrape=league_seasons %>% filter(season>=1974)
  }
  else if (to_scrape %in% c("shooting", "play-by-play")){
    seas_to_scrape=league_seasons %>% filter(season>=1997)
  }
  else if (to_scrape == "per_minute"){
    seas_to_scrape=league_seasons %>% filter(season>=1952)
  }
  else{
    seas_to_scrape=league_seasons
  }
  for (row in 1:nrow(seas_to_scrape)) {
    curr_lg=seas_to_scrape$league[row]
    curr_seas=seas_to_scrape$season[row]
    a=bind_rows(
      a,
      player_stats(league=curr_lg,season=curr_seas,type = to_scrape)
    )
    print(paste(curr_lg,curr_seas))
  }
  a<-a %>% arrange(desc(season),player_id)
  return(a)
}


totals <- get_all_player_stats("totals")

# player season info starts with season, league, player, slug, age, team, pos

# add experience variable
totals_info <- totals %>%
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
write_csv(player_career_info,"Data/Player Career Info.csv")

per_game <- get_all_player_stats("per_game")

advanced <- get_all_player_stats("advanced")

per_36 <- get_all_player_stats("per_minute")

per_100 <- get_all_player_stats("per_poss")

shooting <- get_all_player_stats("shooting")

play_by_play <- get_all_player_stats("play-by-play")

write_csv(advanced, "Data/Advanced.csv")
write_csv(per_100, "Data/Per 100 Poss.csv")
write_csv(per_36, "Data/Per 36 Minutes.csv")
write_csv(totals, "Data/Player Totals.csv")
write_csv(per_game, "Data/Player Per Game.csv")
write_csv(play_by_play, "Data/Player Play By Play.csv")
write_csv(shooting, "Data/Player Shooting.csv")
