library(rvest)
library(tidyverse)
library(curl)
library(jsonlite)

advanced=read_csv("Advanced.csv")
per_game=read_csv("Player Per Game.csv")
award_shares=read_csv("Player Award Shares.csv") %>% 
  group_by(player_id,player,award) %>% summarize(sum(share),sum(winner))
all_nba_teams=read_csv("End of Season Teams.csv") %>% 
  filter(type=="All-NBA" & season>1973) %>% 
  select(season,player_id,team_rank:position,age,tm)
totals=read_csv("Player Totals.csv")
team_summaries=read_csv("Team Summaries.csv") %>% mutate(g_total=w+l) %>% 
  rename(tm=abbreviation) %>% select(season,tm,g_total)

#for nba hometowns, vorp > 0.07 and mp > 10000
all_play_only_tot=all_play %>% mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm))

top_ws_per_48_10000_mins=all_play_only_tot %>% filter(tm !="TOT" & season>1951) %>% group_by(player_id) %>% 
  mutate(career_ws=sum(ws),career_mp=sum(mp)) %>% slice_max(season,n=1) %>% 
  distinct(player_id,.keep_all=TRUE) %>% filter(career_mp >= 10000) %>% 
  mutate(career_ws_per_48=round(career_ws/career_mp*48,digits=4)) %>%
  arrange(desc(career_ws_per_48)) %>% filter(career_ws_per_48>=0.07) %>%
  select(player_id,season,player,hof,career_mp,career_ws_per_48,career_ws)

write_csv(top_ws_per_48_10000_mins,"top_ws_per_48_10000_mins.csv")

debuts_after_1980=all_play_only_tot %>% group_by(player_id) %>% slice_min(season,n=1) %>% 
  distinct(player_id,.keep_all=TRUE) %>% filter(season>1979) %>% select(player_id,player)

write_csv(all_play_only_tot %>% filter(season>1979) %>% group_by(player_id) %>% 
  semi_join(.,debuts_after_1980) %>%
  mutate(across(c(g,mp:fga,x3p,x3pa,ft,fta,trb:tov,pts,ws),list(career=~sum(.)))) %>%
  slice_max(season,n=1) %>% distinct(player_id,.keep_all=TRUE) %>% 
  filter(mp_career>=10000) %>% 
  mutate(fg_perc_career=ifelse(fga_career==0,0,fg_career/fga_career),
         x3p_perc_career=ifelse(x3pa_career==0,0,x3p_career/x3pa_career),
         ft_perc_career=ifelse(fta_career==0,0,ft_career/fta_career)) %>% 
  mutate(across(c(mp_career:pts_career),list(per_game=~round(./g_career,digits=1)))) %>%
  mutate(career_ws_per_48=round(ws_career/mp_career*48,digits=4)) %>% 
  filter(career_ws_per_48>=0.07) %>%
  select(seas_id:hof,g_career,mp_career,ws_career:career_ws_per_48),"for_viz.csv")

#top vorp on every team
top_vorp=left_join(advanced,advanced %>% filter(tm != "TOT" & season>1973) %>%
                     group_by(tm,season) %>% slice_max(vorp,n=1) %>% mutate(top_in_team=1)) %>% 
  ungroup() %>% ungroup() %>% group_by(player_id) %>% 
  mutate(top_times=sum(top_in_team,na.rm = TRUE),career_vorp=sum(vorp)) %>% slice(1) %>% 
  mutate(percent_top=top_times/experience,vorp_per_season=career_vorp/experience) %>%
  arrange(desc(percent_top),desc(vorp_per_season)) %>% 
  select(player_id,player,hof,experience,top_times,percent_top,career_vorp,vorp_per_season)
write_csv(top_vorp,"top_vorp.csv")

#top win shares on every team
top_ws=left_join(advanced,advanced %>% filter(tm != "TOT" & season>1951) %>% 
                 group_by(tm,season) %>% slice_max(per) %>% mutate(top_in_team=1)) %>% 
  ungroup() %>% ungroup() %>% group_by(player_id) %>% 
  mutate(top_times=sum(top_in_team,na.rm = TRUE),career_ws=sum(ws)) %>% slice(1) %>% 
  mutate(percent_top=top_times/experience,ws_per_season=career_ws/experience) %>%
  arrange(desc(percent_top),desc(ws_per_season)) %>% 
  select(player_id,player,hof,experience,top_times,percent_top,career_ws,ws_per_season)

career_vorp=advanced %>% group_by(player_id,player) %>% summarize(career_vorp=sum(vorp,na.rm=TRUE))

#positionless all-nba teams by vorp
since_1989=advanced %>% filter(season %in% (1989:2020) & lg=="NBA") %>% 
  arrange(desc(vorp)) %>% group_by(season) %>% slice_max(vorp,n=15) %>% 
  mutate(good_ranks=rank(desc(vorp),ties.method = "min")) %>%
  mutate(team_rank=case_when(good_ranks %in% 1:5 ~ "1st",
                             good_ranks %in% 6:10 ~ "2nd",
                             good_ranks %in% 11:15 ~ "3rd")) %>%
  select(season,team_rank,player_id,player,pos:experience,tm,g,vorp) %>% ungroup()
between_89_and_73=advanced %>% filter(season %in% (1974:1988) & lg=="NBA") %>% 
  arrange(desc(vorp)) %>% group_by(season) %>% slice_max(vorp,n=10) %>% 
  mutate(good_ranks=rank(desc(vorp),ties.method = "min")) %>%
  mutate(team_rank=case_when(good_ranks %in% 1:5 ~ "1st",
                             good_ranks %in% 6:10 ~ "2nd")) %>%
  select(season,team_rank,player_id,player,pos:experience,tm,g,vorp) %>% ungroup()
since_73=add_row(since_1989,between_89_and_73) %>% arrange(desc(season))
rm(since_1989,between_89_and_73)
differences=full_join(since_73 %>% group_by(player,team_rank,player_id) %>% tally() %>% 
                        rename(num_new=n),
                      all_nba_teams %>% group_by(player,team_rank,player_id) %>% tally() %>% 
                        rename(num_old=n)) %>%
  replace_na(list(num_old = 0, num_new = 0)) %>% mutate(diff=num_new-num_old) %>% 
  left_join(.,career_vorp)
totals=differences %>% group_by(player_id) %>% 
  mutate(tot_old=sum(num_old),tot_new=sum(num_new),tot_diff=tot_new-tot_old) %>% 
  slice(player_id,1) %>% select(player,player_id,career_vorp:tot_diff)

#sixth man winners if sixth to ninth on team in minutes played
smoy_winners=advanced %>% filter(tm !="TOT" & season==2020) %>% left_join(.,per_game) %>%
  left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5) %>% 
  mutate(mp_per_game=mp/g,.after="mp") %>% arrange(desc(mp_per_game)) %>% 
  group_by(tm,season) %>% slice(6:9) %>% group_by(season) %>% slice_max(vorp,n=5) %>% 
  select(season,player,pos:experience,tm,g,gs,mp_per_game,vorp)

smoy_winners_ppg=per_game %>% filter(tm !="TOT" & season==2020) %>% 
  left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5) %>% 
  arrange(desc(mp_per_game)) %>% group_by(tm,season) %>% slice(6:9) %>% 
  group_by(season) %>% slice_max(pts_per_game,n=5) %>% 
  select(season,player,pos:experience,tm,g,mp_per_game,pts_per_game)
write_csv(smoy_winners,"Hypothetical SMOYs.csv")
