######################################################## 
# Install any required packages and setup the dataset
#

# Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#if(!require(glue)) install.packages("glue", repos = "http://cran.us.r-project.org")
#if(!require(hash)) install.packages("hash", repos = "http://cran.us.r-project.org")

# Unpack the dataset
zipfile <- "365722_715072_bundle_archive.zip"
datapath <- "./data"
if (!dir.exists(datapath)) {
  unzip(zipfile, exdir=datapath)
}

set.seed(19, sample.kind="Rounding")

# Load the raw matches data
match_files <- list.files(path = datapath, pattern="atp_matches_[0-9]*.csv")
raw_matches = NULL
for(file in match_files) {
  col_types <- cols(
    tourney_id = col_character(),
    tourney_name = col_character(),
    tourney_date = col_date("%Y%m%d"),
    surface = col_character(),
    winner_id = col_integer(),
    loser_id = col_integer(),
    score = col_character(),
    best_of = col_integer(),
    round = col_character(),
    minutes = col_double()
  )
  matches_data <- read_csv(file.path(datapath, file), col_types=col_types)
  if(is.null(raw_matches)) {
    raw_matches <- matches_data
  }
  else {
    raw_matches <- bind_rows(raw_matches, matches_data)
  }
}

# Filter out walkovers and matches with missing info
atp_matches <- raw_matches %>%
  mutate(surface=factor(surface),
         round=factor(round)) %>%
  filter(!surface %in% c('None', NA)
         & !is.na(w_ace+w_df+w_svpt+w_1stIn+w_1stWon+w_2ndWon+w_SvGms+w_bpSaved+w_bpFaced
                  +l_ace+l_df+l_svpt+l_1stIn+l_1stWon+l_2ndWon+l_SvGms+l_bpSaved+l_bpFaced)
         & str_detect(score, "[0-9]")) %>%
  mutate(retired = str_detect(score, regex("ret", ignore_case=TRUE))) %>%
  select(-score, -winner_rank, -winner_rank_points, -loser_rank, -loser_rank_points) %>%
  mutate(ptWonRate = (w_1stWon + w_2ndWon + l_svpt - l_1stWon - l_2ndWon)/(w_svpt + l_svpt))


# Randomize which player is listed first
shuffle_matches <- function(match_data) {
  swap <- sample(c(TRUE, FALSE), size=(nrow(match_data)), replace=TRUE)
  match_data %>%
    rename(player1_id="winner_id", player2_id="loser_id") %>%
    mutate(winner_id=player1_id) %>%
    mutate(win=ifelse(!swap, "Win", "Lose")) %>%
    mutate(win=factor(win)) %>%
    select(tourney_id, tourney_name, tourney_date, surface, best_of, player1_id, player2_id, winner_id, win) %>%
    mutate(player1_id=ifelse(swap, player2_id, player1_id),
           player2_id=ifelse(swap, winner_id, player2_id))
}

# Load the player data
atp_players <- read_csv(file.path(datapath, "atp_players.csv"), col_types=cols(
  player_id = col_integer(),
  name_first = col_character(),
  name_list = col_character(),
  hand = col_factor(),
  birthdate = col_date("%Y%m%d"),
  country = col_factor()
))


final_end_date = "2018-01-01"
end_date = "2017-01-01"
final_start_date = end_date
start_date = "2016-01-01"

# Split into training and test sets
matches_final_train <- atp_matches %>%
  filter(tourney_date < final_start_date)

matches_train <- matches_final_train %>%
  filter(tourney_date < start_date)

# Get all players in test sets
known_final_players <- matches_final_train %>%
  gather(key="player_type", value="player_id", winner_id, loser_id) %>%
  distinct(player_id) %>%
  .$player_id
known_players <- matches_train %>%
  gather(key="player_type", value="player_id", winner_id, loser_id) %>%
  distinct(player_id) %>%
  .$player_id


matches_final_test <- atp_matches %>%
  filter(winner_id %in% known_final_players
         & loser_id %in% known_final_players) %>%
  filter(tourney_date >= final_start_date
         & tourney_date < final_end_date)
matches_final_test <- shuffle_matches(matches_final_test)

matches_test <- matches_final_train %>%
  filter(winner_id %in% known_players
         & loser_id %in% known_players) %>%
  filter(tourney_date >= start_date
         & tourney_date < end_date)
matches_test <- shuffle_matches(matches_test)


# Make table with every match having an entry from each player's perspectives
neutralize_matches <- function(match_data) {
  winners <- match_data %>%
    rename(player_id = "winner_id",
           opponent_id = "loser_id",
           p_ace = "w_ace",
           p_df = "w_df",
           p_svpt = "w_svpt",
           p_1stIn = "w_1stIn",
           p_1stWon = "w_1stWon",
           p_2ndWon = "w_2ndWon",
           p_SvGms = "w_SvGms",
           p_bpSaved = "w_bpSaved",
           p_bpFaced = "w_bpFaced",
           o_ace = "l_ace",
           o_df = "l_df",
           o_svpt = "l_svpt",
           o_1stIn = "l_1stIn",
           o_1stWon = "l_1stWon",
           o_2ndWon = "l_2ndWon",
           o_SvGms = "l_SvGms",
           o_bpSaved = "l_bpSaved",
           o_bpFaced = "l_bpFaced") %>%
    mutate(win="Win")
  losers <- match_data %>%
    rename(player_id = "loser_id",
           opponent_id = "winner_id",
           p_ace = "l_ace",
           p_df = "l_df",
           p_svpt = "l_svpt",
           p_1stIn = "l_1stIn",
           p_1stWon = "l_1stWon",
           p_2ndWon = "l_2ndWon",
           p_SvGms = "l_SvGms",
           p_bpSaved = "l_bpSaved",
           p_bpFaced = "l_bpFaced",
           o_ace = "w_ace",
           o_df = "w_df",
           o_svpt = "w_svpt",
           o_1stIn = "w_1stIn",
           o_1stWon = "w_1stWon",
           o_2ndWon = "w_2ndWon",
           o_SvGms = "w_SvGms",
           o_bpSaved = "w_bpSaved",
           o_bpFaced = "w_bpFaced") %>%
    mutate(win="Lose")
  
  bind_rows(winners, losers)
}



