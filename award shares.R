library(rvest)
library(tidyverse)
library(janitor)
library(polite)

bbref_bow=bow("https://www.basketball-reference.com/",user_agent = "Sumitro Datta",force=TRUE)
print(bbref_bow)

get_award_pcts <- function(season=2020,award="mvp",lg="nba"){
  # scrape
  session=nod(bbref_bow,path=paste0("awards/awards_",season,".html")) %>% scrape()
  awarding <- award
  # aba mvp awarded from 1968-1976
  if (award == "mvp") {
    if (season <= 1976 & season >= 1968) {
      awarding <- paste0(lg, "_", award)
    }
  } else if (award=="roy") # aba roy has voting results from 1972 to 1975 (other years added later)
  {
    if (season <= 1975 & season >= 1972) {
      awarding <- paste0(lg, "_", award)
    }
  }
  
  raw_table=tibble()
  if (award %in% c("mvp","roy")){
    raw_table=session %>% html_elements(css=paste0("#",awarding)) %>% 
      html_table() %>% .[[1]]
    
    award_player_ids=tibble(
      player_id=session %>% 
        html_element(paste0('table#',awarding)) %>%
        html_elements("td") %>%
        #data-append-csv have slugs
        html_attr("data-append-csv")) %>% 
      filter(!is.na(player_id))
    
  } else { #other awards in comment tables
    reparsed_html=session %>% 
      # thanks to alistaire (https://stackoverflow.com/questions/40616357/how-to-scrape-tables-inside-a-comment-tag-in-html-with-r/40616937#40616937)
      html_elements(xpath = '//comment()') %>%    # select comment nodes
      html_text() %>%    # extract comment text
      paste(collapse = '') %>%    # collapse to a single string
      read_html() %>%    # reparse to HTML
      html_element(paste0('table#',awarding))    # select the desired table
    
    raw_table=reparsed_html %>% html_table()
    
    award_player_ids=tibble(
      player_id=
        reparsed_html %>% 
        html_elements("td") %>%
        #data-append-csv have slugs
        html_attr("data-append-csv")) %>% 
      filter(!is.na(player_id))
  }
  
  final_table=raw_table %>%
    # first row is actual column names
    row_to_names(1) %>% clean_names() %>% 
    # select only voting columns (no stats)
    select(player:share) %>%
    mutate(season=season,award=paste(lg,award),.before="player") %>%
    mutate(across(c(age,first:share),as.numeric)) %>% 
    mutate(winner=if_else(share==max(share),TRUE,FALSE)) %>%
    bind_cols(.,award_player_ids) %>% 
    relocate(player_id,.after=player) %>%
    select(-tm)
  
  return(final_table)
}

all_lg_voting<- function(season=2020,award="all_nba",league="nba"){
  session=nod(bbref_bow,path=paste0("awards/awards_",season,".html")) %>% scrape()
  
  raw_table <- session %>% 
    html_elements(css=paste0("#all_leading_",award)) %>% 
    html_table() %>% .[[1]] %>% 
    # first row is actual column names
    row_to_names(1) %>% clean_names() %>% 
    # remove blank rows
    filter(player!="") %>%
    # remove asterisks (players with notes)
    mutate(player=ifelse(str_detect(player,"\\*"),str_sub(player,end=-2),player),
           season=season,lg=league,type=award,.before=everything()) %>%
    select(-tm)
  
  all_lg_voting_player_ids=tibble(
    player_id=session %>% 
      html_elements(css=paste0("#all_leading_",award)) %>%
      html_elements("td") %>%
      #data-append-csv have slugs
      html_attr("data-append-csv")) %>% 
    filter(!is.na(player_id))
  
  if (award !="all_rookie"){
    raw_table <- raw_table %>% rename(position=pos)
  } 
  if (award=="all_defense"){
    # select only voting columns (no stats)
    voting_table=raw_table %>% select(season:share) %>% 
      mutate(across(c(age:share),as.numeric))
  } else{
    # select only voting columns (no stats)
    voting_table=raw_table %>% select(-c(g:ws_48)) %>% 
      mutate(across(-c(season:player),as.numeric))
  }
  
  final_table=bind_cols(voting_table,all_lg_voting_player_ids) %>% 
    relocate(player_id,.after=player)
  
  return(final_table)
}

end_seas_team_scrape <- function(type="all_league"){
  session=nod(bbref_bow,path=paste0("awards/",type,".html")) %>% scrape()
  all_lg <- session %>% 
    html_elements(css = paste0("#awards_",type)) %>%
    html_table() %>% .[[1]] %>% 
    rename(season=1,lg=2,number_tm=3,voting=4,
           player1=5,player2=6,player3=7,player4=8,player5=9) %>% 
    filter(player1 !="") %>% 
    mutate(season = as.numeric(substr(season, 0, 4)) + 1) %>% 
    select(-voting) %>%
    # players become one column
    pivot_longer(-c(season, lg, number_tm), values_to="player") %>%
    select(-name) %>%
    # separate out ties
    separate_rows(player, sep = ", ") %>% 
    mutate(player = ifelse(str_detect(player, "\\(T\\)$"), 
                           substr(player, 1, nchar(player) - 4), 
                           str_trim(player))) %>% 
    mutate(
      with_pos=str_detect(player, "(C|F|G)$"),
      position=if_else(with_pos,str_sub(player, start = -1),NA),
      player=if_else(with_pos,substr(player, 1, nchar(player) - 2),player)) %>%
    mutate(type = case_when(type=="all_league"~paste("All", lg, sep = "-"),
                            type=="all_defense"~"All-Defense",
                            type=="all_rookie"~"All-Rookie"),.after=lg) %>% 
    select(-with_pos)
  
  all_league_player_ids=tibble(player_id=session %>% 
                            html_elements(css = paste0("#awards_",type)) %>%
                            html_elements("td") %>%
                            html_elements("a") %>%
                            html_attr("href")) %>%
    filter(str_detect(player_id,"players")) %>%
    mutate(player_id=str_remove(word(player_id,sep="/",start=4),".html"))
  
  final_all_league=bind_cols(all_lg,all_league_player_ids) %>% 
    relocate(player_id,.after=player)
}


all_stars <- function(season = 2020, league = "NBA") {
  session=nod(bbref_bow,paste0("leagues/", league, "_", season, ".html")) %>% scrape()
  
  reparsed_html= session %>%
    html_elements(xpath = "//comment()") %>%
    html_text() %>%
    paste(collapse = "") %>%
    read_html() %>%
    html_elements(xpath = paste0('//*[(@id = "div_all_star_game_rosters")]')) %>% 
    html_elements("table")
  team_names <- reparsed_html %>%
    html_elements("caption") %>%
    html_text()
  rosters <- reparsed_html %>%
    html_elements("tr") %>%
    html_text() %>%
    str_trim(.) %>%
    str_split(., "\\s{2,100}")
  
  all_stars_tibble <- tibble(player = character(), team = character(), season = integer(), lg = character())
  for (i in 1:length(team_names)){
    all_stars_tibble=bind_rows(
      all_stars_tibble,
      tibble(player=rosters[[i]],team=team_names[[i]],lg=league,season=season)
    )
  }
  
  all_star_player_ids=tibble(
    player_id=
      reparsed_html %>% 
      html_elements("td") %>%
      html_elements("a") %>%
      html_attr("href")) %>%
    filter(str_detect(player_id,"players")) %>%
    mutate(player_id=str_remove(word(player_id,sep="/",start=4),".html"))
  
  final_all_stars=bind_cols(all_stars_tibble,all_star_player_ids) %>%
    mutate(
      replaced = str_detect(player, "\\("),
      hof = str_detect(player, "\\*"),
      player = ifelse(replaced, substr(player, 1, nchar(player) - 4), player),
      player = ifelse(hof, substr(player, 1, nchar(player) - 1), player)
    ) %>%
    relocate(player_id,.after=player)
  
  return(final_all_stars)
}
