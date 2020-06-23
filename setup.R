######################################################## 
# Install any required packages and setup the dataset
#

# Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#if(!require(glue)) install.packages("glue", repos = "http://cran.us.r-project.org")

# Unpack the dataset
zipfile <- "365722_715072_bundle_archive.zip"
datapath <- "./data"
if (!dir.exists(datapath)) {
  unzip(zipfile, exdir=datapath)
}

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

reformat_matches <- function(raw_match_data) {
}

# For whatever reason glue trim is not working, so
# this is necessary
trim <- function(x) {
  str_replace(str_replace(x, "^\\s+", ""), "\\s+$", "")
}

valid_matches <- raw_matches %>%
  mutate(surface=factor(surface),
         round=factor(round)) %>%
  filter(!surface %in% c('None', NA)
         & !is.na(w_ace+w_df+w_svpt+w_1stIn+w_1stWon+w_2ndWon+w_SvGms+w_bpSaved+w_bpFaced
                  +l_ace+l_df+l_svpt+l_1stIn+l_1stWon+l_2ndWon+l_SvGms+l_bpSaved+l_bpFaced)
         & str_detect(score, "[0-9]")) %>%
  # filter(str_detect(score, score_regex)) %>%
  mutate(a_retired = FALSE,
         o_retired = str_detect(score, regex("ret", ignore_case=TRUE))) %>%
  select(-score, -winner_rank, -winner_rank_points, -loser_rank, -loser_rank_points)


lead_columns = c("tourney_id", "tourney_name", "tourney_date", "athlete_id", "opponent_id", "won", "winner_id", "surface", "best_of", "round", "minutes")

# Make information about who won easily removable and add missing info
reformatted_matches <- valid_matches %>%
  rename(athlete_id="winner_id",
         opponent_id="loser_id",
         a_ace="w_ace",
         o_ace="l_ace",
         a_df="w_df",
         o_df="l_df",
         a_svpt="w_svpt",
         o_svpt="l_svpt",
         a_sv1stIn="w_1stIn",
         o_sv1stIn="l_1stIn",
         a_sv1stWon="w_1stWon",
         o_sv1stWon="l_1stWon",
         a_sv2ndWon="w_2ndWon",
         o_sv2ndWon="l_2ndWon",
         a_SvGms="w_SvGms",
         o_SvGms="l_SvGms",
         a_svbpSaved="w_bpSaved",
         o_svbpSaved="l_bpSaved",
         a_svbpFaced="w_bpFaced",
         o_svbpFaced="l_bpFaced") %>%
  mutate(won=TRUE, winner_id=athlete_id) %>%
  mutate(a_frac_sv1stIn = a_sv1stIn / a_svpt,
         a_frac_sv1stWon = a_sv1stWon / a_sv1stIn,
         a_frac_sv2ndWon = a_sv2ndWon / (a_svpt - a_sv1stIn),
         a_frac_svbpSaved = a_svbpSaved / a_svbpFaced,
         o_frac_sv1stIn = o_sv1stIn / o_svpt,
         o_frac_sv1stWon = o_sv1stWon / o_sv1stIn,
         o_frac_sv2ndWon = o_sv2ndWon / (o_svpt - o_sv1stIn),
         o_frac_svbpSaved = o_svbpSaved / o_svbpFaced,
         a_frac_rt1stWon = 1 - o_frac_sv1stWon,
         a_frac_rt2ndWon = 1 - o_frac_sv2ndWon,
         a_frac_rtbpConv = 1 - o_frac_svbpSaved,
         o_frac_rt1stWon = 1 - a_frac_sv1stWon,
         o_frac_rt2ndWon = 1 - a_frac_sv2ndWon,
         o_frac_rtbpConv = 1 - a_frac_svbpSaved,
         a_svptWon = a_sv1stWon+a_sv2ndWon,
         a_frac_svptWon = (a_svptWon) / a_svpt,
         o_svptWon = o_sv1stWon+o_sv2ndWon,
         o_frac_svptWon = (o_svptWon) / o_svpt,
         a_rtptWon = o_svpt - o_svptWon,
         a_frac_rtptWon = a_rtptWon / o_svpt,
         o_rtptWon = a_svpt - a_svptWon,
         o_frac_rtptWon = o_rtptWon / a_svpt,
         a_ptWon = a_svptWon + a_rtptWon,
         o_ptWon = o_svptWon + o_rtptWon,
         a_frac_ptWon = a_ptWon / (a_ptWon + o_ptWon),
         o_frac_ptWon = o_ptWon / (a_ptWon + o_ptWon)) %>%
  select(noquote(append(lead_columns, sort(setdiff(colnames(.), lead_columns)))))

reversed_matches <- reformatted_matches %>%

colnames(reformatted_matches)   

#.[,c(1,2,3)]
#reordered_matches <- reformatted_matches[,c(1:3,5,6,30,31,4,7:9,28,10:18,46,50,54,32:35,40,41,)]         
                  
         
                 

# Reformat the matches data for usability and remove incomplete data


# Load the player data
atp_players <- read_csv(file.path(datapath, "atp_players.csv"), col_types=cols(
  player_id = col_integer(),
  name_first = col_character(),
  name_list = col_character(),
  hand = col_factor(),
  birthdate = col_date("%Y%m%d"),
  country = col_factor()
))

# Split into training and test sets
matches_final_train <- atp_matches %>%
  filter(tourney_date < "2018-01-01")

matches_final_test <- atp_matches %>%
  filter(tourney_date >= "2018-01-01")

matches_train <- matches_final_train %>%
  filter(tourney_date < "2017-01-01")

matches_test <- matches_final_train %>%
  filter(tourney_date >= "2017-01-01")

