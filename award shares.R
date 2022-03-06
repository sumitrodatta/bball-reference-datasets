library(rvest)
library(tidyverse)
library(janitor)
library(polite)

bbref_bow=bow("https://www.basketball-reference.com/",user_agent = "Sumitro Datta",force=TRUE)
print(bbref_bow)

# works with mvp, roy
get_award_pcts_mvp_roy <- function(season = 2020, award = "mvp", lg = "nba") {
  session=nod(bbref_bow,path=paste0("awards/awards_",season,".html"))
  awarding <- award
  # aba mvp awarded from 1968-1976
  if (award == "mvp") {
    if (season < 1977 & season > 1967) {
      awarding <- paste0(lg, "_", award)
    }
  }
  else if (award=="roy") # aba roy has voting results from 1972 to 1975 (other years added later)
  {
    if (season < 1976 & season > 1971) {
      awarding <- paste0(lg, "_", award)
    }
  }
  pcts <- scrape(session) %>% html_nodes(css=paste0("#all_",awarding)) %>% 
    html_table() %>% .[[1]] %>%
    # first row is actual column names
    row_to_names(1) %>% clean_names() %>% 
    # select only voting columns (no stats)
    select(player:share) %>% 
    # add season awarded and award name
    add_column(season = season, .before = "player") %>%
    add_column(award = paste(lg, award), .before = "player") %>%
    add_column(winner = FALSE) %>%
    mutate(across(c(age,first:share),as.numeric))
  return(pcts)
}

# works with dpoy, smoy, mip (all there are nba-only awards)
get_award_pcts_other <- function(season = 2020, award = "mip") {
  session=nod(bbref_bow,path=paste0("awards/awards_",season,".html"))
  pcts <- scrape(session) %>% 
    # thanks to Carl Boneri (https://stackoverflow.com/questions/40616357/how-to-scrape-tables-inside-a-comment-tag-in-html-with-r/40616937#40616937)
    html_nodes(xpath = '//comment()') %>%    # select comment nodes
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%    # collapse to a single string
    read_html() %>%    # reparse to HTML
    html_node(paste0('table#',award)) %>%    # select the desired table
    html_table() %>%
    # first row is actual column names
    row_to_names(1) %>% clean_names() %>% 
    # select only voting columns (no stats)
    select(player:share) %>% 
    # add season awarded and award name
    add_column(season = season, .before = "player") %>%
    add_column(award = award, .before = "player") %>%
    mutate(winner = FALSE) %>%
    mutate(across(c(age,first:share),as.numeric))
  return(pcts)
}

all_lg_voting<- function(season=2020){
  session=nod(bbref_bow,path=paste0("awards/awards_",season,".html"))
  pcts <- scrape(session) %>% html_nodes(css="#all_leading_all_nba") %>% 
    html_table() %>% .[[1]] %>% 
    # first row is actual column names
    row_to_names(1) %>% clean_names() %>% 
    # remove blank rows
    filter(player!="") %>%
    # remove asterisks (players with notes)
    mutate(player=ifelse(str_detect(player,"\\*"),str_sub(player,end=-2),player)) %>%
    # select only voting columns (no stats)
    select(number_tm:x3rd_tm) %>% 
    mutate(season=season,lg="NBA",type="All-NBA",.before=everything()) %>%
    rename(position=pos) %>%
    mutate(across(c(age,pts_won:x3rd_tm),as.numeric))
  return(pcts)
}

# get all-league teams
all_lg_scrape <- function() {
  session=nod(bbref_bow,path="awards/all_league.html")
  all_lg <- scrape(session) %>% 
    html_nodes(css = "#div_awards_all_league") %>%
    html_table() %>% .[[1]]
  colnames(all_lg) <- c("Season", "Lg", "number_tm", "Voting", "Player1", "Player2", "Player3", "Player4", "Player5")
  all_lg <- all_lg %>% filter(Player1 != "") %>% 
    mutate(Season = as.numeric(substr(Season, 0, 4)) + 1) %>% 
    select(-Voting) %>%
    # players become one column
    pivot_longer(-c(Season, Lg, number_tm), names_prefix = "Player",values_to="Player") %>%
    select(-name) %>%
    # separate out ties
    separate_rows(Player, sep = ", ") %>% 
    mutate(Player = ifelse(str_detect(Player, "\\(T\\)$"), substr(Player, 1, nchar(Player) - 4), str_trim(Player)))
  # separate out positions
  nopos <- all_lg %>%
    filter(!str_detect(Player, "(C|F|G)$")) %>%
    mutate(Position = NA)
  with_pos <- all_lg %>%
    filter(str_detect(Player, "(C|F|G)$")) %>%
    mutate(Position = str_sub(Player, start = -1), Player = substr(Player, 1, nchar(Player) - 2))
  final_all_lg <- rbind(with_pos, nopos) %>%
    mutate(Type = paste("All", Lg, sep = "-")) %>%
    select(1, 2, 6, 3, 4, 5) %>%
    clean_names()
  return(final_all_lg)
}

all_def_or_all_rookie <- function(type = "all_defense") {
  session=nod(bbref_bow,path=paste0("awards/", type, ".html"))
  def_or_rookie <- scrape(session) %>% 
    html_nodes(css = paste0("#div_awards_",type)) %>%
    html_table() %>% .[[1]]
  colnames(def_or_rookie) <- c("Season", "Lg", "number_tm", "Player1", "Player2", "Player3", "Player4", "Player5")
  def_or_rookie <- def_or_rookie %>% filter(Player1 !="") %>%
    mutate(Season = as.numeric(substr(Season, 0, 4)) + 1) %>% 
    select(-Voting) %>%
    # players become one column
    pivot_longer(-c(Season, Lg, number_tm), names_prefix = "Player",values_to="Player") %>%
    select(-name) %>%
    # separate out ties
    separate_rows(Player, sep = ", ") %>% 
    mutate(Player = ifelse(str_detect(Player, "\\(T\\)$"), substr(Player, 1, nchar(Player) - 4), str_trim(Player))) %>% 
    arrange(desc(Season), Lg, number_tm)
  if (type == "all_defense") {
    def_or_rookie <- def_or_rookie %>% add_column(Type = "All-Defense", .before = "number_tm")
  }
  else {
    def_or_rookie <- def_or_rookie %>% add_column(Type = "All-Rookie", .before = "number_tm")
  }
  def_or_rookie <- def_or_rookie %>%
    mutate(Position = NA) %>%
    clean_names()
  return(def_or_rookie)
}

all_stars <- function(season = 2020, league = "NBA") {
  session=nod(bbref_bow,paste0("leagues/", league, "_", season, ".html"))
  new_season <- scrape(session) %>%
    html_nodes(xpath = "//comment()") %>%
    html_text() %>%
    paste(collapse = "") %>%
    read_html() %>%
    html_nodes(xpath = paste0('//*[(@id = "div_all_star_game_rosters")]')) %>%
    html_nodes("table")
  team_names <- new_season %>%
    html_nodes("caption") %>%
    html_text()
  rosters <- new_season %>%
    html_nodes("tr") %>%
    html_text() %>%
    str_trim(.) %>%
    str_split(., "\\s{2,100}")
  all_stars_tibble <- tibble(player = character(), team = character(), season = integer(), lg = character())
  team1 <- tibble(player = rosters[[1]]) %>% mutate(team = team_names[[1]], lg = league, season = season)
  team2 <- tibble(player = rosters[[2]]) %>% mutate(team = team_names[[2]], lg = league, season = season)
  all_stars_tibble <- rbind(all_stars_tibble, team1) %>% rbind(., team2)
  return(all_stars_tibble)
}
