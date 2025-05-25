library(rvest)
library(tidyverse)
library(janitor)
library(polite)

bbref_bow=bow("https://www.basketball-reference.com/",user_agent = "Sumitro Datta",force=TRUE,delay=10)
print(bbref_bow)

# per_game-team, per_game-opponent
# totals-team, totals-opponent
# per_poss-team, per_poss-opponent from 1974
# advanced-team
team_stats <- function(season = 2020, league = "NBA", type = "per_game-team") {
  session = nod(bbref_bow,path=paste0("leagues/", league, "_", season, ".html")) %>% scrape()
  new_season <- session %>% 
    html_elements(css=paste0("#",type)) %>%
    html_table() %>% .[[1]]
  
  abbrevs=tibble(href=session %>% 
                   html_elements(css=paste0("#",type)) %>% 
                   #link in table to team's specific page
                   html_elements("a") %>% 
                   html_attr("href")) %>%
    #format is /teams/[abbrev]/[year].html
    mutate(abbreviation=word(href,start=3,sep="/")) %>% 
    select(abbreviation) %>%
    #league average is not a team, so no abbreviation
    add_row(abbreviation=NA)
  
  if (type=="advanced-team"){
    new_season <- new_season %>% 
      #advanced table has first header for offense & defense four factors
      row_to_names(1) %>% 
      clean_names() %>% 
      #advanced has 3 blank separator columns
      select(!starts_with("na")) %>%
      #defensive four factors are opponent stats
      rename_with(~str_c("opp_",str_remove(.,"_2")),ends_with("_2")) %>%
      #attendance has commas
      mutate(attend = gsub(",", "", attend), attend_g = gsub(",", "", attend_g)) %>%
      mutate(across(c(age:opp_ft_fga,attend:attend_g),as.numeric))
  }
  new_season <- new_season %>% clean_names() %>%
    mutate(season=season,lg=league,.before=team) %>%
    select(-rk) %>%
    #separate playoff asterisk into separate column
    mutate(
      playoffs = str_detect(team, "\\*"),
      team = ifelse(playoffs, str_remove(team,"\\*"), team)
    ) %>%
    relocate(playoffs, .after = team)
  if (str_detect(type,"opponent")){
    new_season <- new_season %>%
      rename_with(~str_c("opp_",.),.cols=c(fg:pts))
  }
  if (str_detect(type,"per_game")){
    new_season <- new_season %>%
      rename_with(~str_c(.,"_per_game"),.cols=-c(season:g,contains("percent")))
  }
  if (str_detect(type,"per_poss")){
    new_season <- new_season %>%
      rename_with(~str_c(.,"_per_100_poss"),.cols=-c(season:mp,contains("percent")))
    abbrevs = abbrevs %>% filter(!is.na(abbreviation))
  }

  final_new_season=bind_cols(new_season,abbrevs) %>% 
    relocate(abbreviation, .after = "team")
  
  return(final_new_season)
}

# totals, per_game, per_minute, advanced
# per_poss from 1974
# shooting, play-by-play from 1997
player_stats <- function(season = 2017, league = "NBA", type = "totals") {
  # scrape
  session = nod(bbref_bow,path=paste0("leagues/", league, "_", season, "_", type, ".html")) %>% scrape()
  stats_a <- session %>%
    html_table() %>% .[[1]]
  if (type %in% c("shooting", "play-by-play")) {
    stats_a <- stats_a %>% row_to_names(1)
  }
  # clean
  stats_a <- stats_a %>%
    clean_names() %>%
    select(-awards) %>%
    filter(player != "League Average")
  player_stats_a <- stats_a %>% 
    #convert blanks in position column to NAs
    mutate(pos=na_if(pos,"")) %>%
    mutate(across(-c(player, team, pos), as.numeric)) %>%
    select(-rk) %>%  
    mutate(season=season,lg=league,.before=everything())
  if (str_detect(type,"per_game")){
    if (season<=1970||(season<=1971 && league=="ABA")){
      #add games started column if does not exist for early years
      player_stats_a <- player_stats_a %>% mutate(gs=NA_real_,.after=g)
    }
    player_stats_a <- player_stats_a %>% 
      rename_with(~str_c(.,"_per_game"),
                  .cols=-c(season:gs,contains("percent")))
  }
  if (str_detect(type,"per_minute")){
    player_stats_a <- player_stats_a %>% 
      rename_with(~str_c(.,"_per_36_min"),
                  .cols=-c(season:mp,contains("percent")))
  }
  if (str_detect(type,"per_poss")){
    player_stats_a <- player_stats_a %>% 
      rename_with(~str_c(.,"_per_100_poss"),
                  .cols=-c(season:mp,contains("percent"),contains("rtg")))
  }
  if (str_detect(type,"shooting")){
    player_stats_a <- player_stats_a %>% 
      rename(avg_dist_fga = dist) %>%
      rename_with(.cols=x2p:x3p,.fn= ~ str_c("percent_fga_from_", ., "_range")) %>%
      rename_with(.cols=x2p_2:x3p_2,.fn= ~ paste0("fg_percent_from_", str_remove(.,"_2"), "_range")) %>%
      rename_with(.cols=x2p_3:x3p_3,.fn= ~ paste0("percent_assisted_", str_remove(.,"_3"), "_fg")) %>%
      rename(
        percent_dunks_of_fga = percent_fga, num_of_dunks = number,
        percent_corner_3s_of_3pa = percent_3pa, corner_3_point_percent = x3p_percent,
        num_heaves_attempted = att, num_heaves_made = md
      )
  }
  if (str_detect(type,"play-by-play")){
    if (season<=2005){
      #add offensive fouls drawn column if does not exist
      player_stats_a <- player_stats_a %>% mutate(off_2=NA_real_,.after=shoot_2)
    }
    player_stats_a <- player_stats_a %>%
      rename(
        on_court_plus_minus_per_100_poss = on_court,
        net_plus_minus_per_100_poss = on_off, 
        bad_pass_turnover = bad_pass, lost_ball_turnover = lost_ball, 
        shooting_foul_committed = shoot, offensive_foul_committed = off, 
        shooting_foul_drawn = shoot_2, offensive_foul_drawn = off_2, 
        points_generated_by_assists = pga, fga_blocked = blkd
      )
  }
  stat_player_ids=tibble(
    player_id=session %>% 
      html_element("table") %>%
      html_elements("td") %>%
      #data-append-csv have slugs
      html_attr("data-append-csv")) %>% 
    filter(!is.na(player_id))
  
  final_player_stats_a <- bind_cols(player_stats_a,stat_player_ids) %>% 
    relocate(player_id,.after=player)
  
  return(final_player_stats_a)
}

get_rookie_debuts<-function(season = 2017, league = "NBA"){
  session = nod(bbref_bow,path=paste0("leagues/", league, "_", season, "_rookies.html")) %>% scrape()
  
  debuts=session %>% html_table() %>% .[[1]] %>% 
    row_to_names(1) %>% clean_names() %>% select(-rk) %>%
    #filter out blank or repeated header rows
    filter(!(player %in% c("","Player"))) %>% 
    mutate(season=season,league=league,.before=everything()) %>%
    mutate(
      #remove asterisk from hall of famers
      player = ifelse(str_detect(player, "\\*"), substr(player, 1, nchar(player) - 1), player),
      #take only date part of debut, not game, convert to tidy-friendly date
      #since year is two-digits, using mdy guesses wrong for early years (1946 becomes 2046)
      debut=parse_date_time2(word(debut,end=2,sep=","),"mdy",cutoff_2000 = 45)) %>%
    select(season:debut)
  
  debut_player_ids=tibble(
    player_id=session %>% 
      html_elements("td") %>%
      #data-append-csv have slugs
      html_attr("data-append-csv")) %>% 
    filter(!is.na(player_id))
  
  rookies=bind_cols(debuts,debut_player_ids)
  
  return(rookies)
}

get_letter_directory_table<-function(letter="a"){
  page=nod(bbref_bow,path=paste0("/players/",letter)) %>% scrape()
  
  letter_table=page %>% html_table() %>% .[[1]] %>% clean_names() %>% 
    mutate(
      #hall of famers suffixed with asterisk
      hof = str_detect(player, "\\*"),
      #remove asterisk from hall of famers
      player = ifelse(hof, substr(player, 1, nchar(player) - 1), player),
      #convert blank strings in college to NAs
      colleges=na_if(colleges,""),
      #convert birthdate to tidy friendly format
      birth_date=mdy(birth_date)
      ) %>%
    #height listed as "x-y", where x is feet and y is inches
    #separate using -, multiply feet by 12 and add inches to get height in inches
    mutate(ht_in_in=as.numeric(word(ht,sep="-"))*12+
             as.numeric(word(ht,sep="-",start=2)),.before=wt) %>%
    select(-ht)
  
  letter_player_ids=tibble(
    player_id=page %>% 
      #look for table headers
      html_elements("th") %>%
      #look for links in table headers
      html_elements("a") %>% 
      html_attr("href")) %>%
    #hrefs in "/players/[letter]/[slug].html
    #extract only slug portion
    mutate(player_id=word(word(player_id,sep="/",start=4),sep=".html"))
  
  full_letter_table=bind_cols(letter_table,letter_player_ids)
  
  return(full_letter_table)
}

get_player_directory<-function(){
  session = nod(bbref_bow, path = "players/")
  player_letter_hyperlinks = tibble(
    href = scrape(session) %>% html_elements("a") %>% html_attr("href")) %>%
    #filter for only /players/[letter] links
    filter(str_detect(href, "/players/[a-z]/$")) %>%
    #extract only letter
    mutate(href = word(href, sep = "/", start = 3)) %>% pull(href)
  player_table = tibble()
  for (letter_link in player_letter_hyperlinks) {
    letter_table = get_letter_directory_table(letter_link)
    
    player_table = bind_rows(player_table, letter_table)
    print(letter_link)
  }
  return(player_table)
}

get_draft_picks<-function(season=2020,league="NBA"){
  session = nod(bbref_bow,path=paste0("draft/", league, "_", season, ".html")) %>% scrape()
  
  new_season <- session %>% 
    html_elements(css="#stats") %>%
    html_table() %>% .[[1]] %>% row_to_names(1) %>% 
    select(Rk:College) %>% clean_names() %>% 
    mutate(across(rk:pk,as.numeric)) %>%
    mutate(season=season,lg=league,.before="rk") %>%
    rename(overall_pick=pk) %>% 
    mutate(college=na_if(college,"")) %>%
    #headers repeat before each round
    filter(tm != "Tm") %>% 
    #if other picks not related to specific round, give temp -1
    mutate(round=case_when(college=="Other Picks"~-1,
                           TRUE~as.numeric(word(player,2)))) %>% 
    #fill round number down, 1st round lost in row_to_names
    fill(round) %>% replace_na(list(round=1)) %>% 
    mutate(round=na_if(round,-1),player=na_if(player,"")) %>%
    #any non-player name filtered out
    filter(str_detect(player,"(Round )|(Other Picks)",negate=TRUE)) %>%
    filter(!is.na(player)) %>%
    select(-rk)
  
  draft_player_ids=tibble(player_id=session %>% 
                            html_elements(css="#stats") %>% 
                            #link in table to team's specific page
                            html_elements("a") %>% 
                            html_attr("href")) %>%
    #filter for only /players/[letter] links
    filter(str_detect(player_id, "/players/[a-z]/")) %>%
    mutate(player_id=word(word(player_id,sep="/",start=4),sep=".html"))
  
  full_draft_info=bind_cols(new_season,draft_player_ids) %>% 
    arrange(round,overall_pick) %>%
    relocate(round,.after=overall_pick) %>%
    relocate(player_id,.after=player)
  
  return(full_draft_info)
}
