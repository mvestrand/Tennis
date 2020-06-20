######################################################## 
# Install any required packages and setup the dataset
#

# Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


# Unpack the dataset
zipfile <- "365722_715072_bundle_archive.zip"
datapath <- "./data"
if (!dir.exists(datapath)) {
  unzip(zipfile, exdir=datapath)
}

# Load the matches data
match_files <- list.files(path = datapath, pattern="atp_matches_[0-9]*.csv")
atp_matches = NULL
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
  if(is.null(atp_matches)) {
    atp_matches <- matches_data
  }
  else {
    atp_matches <- bind_rows(atp_matches, matches_data)
  }
}
atp_matches <- atp_matches %>%
  mutate(surface=factor(surface))
         

# Load the player data
atp_players <- read_csv(file.path(datapath, "atp_players.csv"), col_types=cols(
  player_id = col_integer(),
  name_first = col_character(),
  name_list = col_character(),
  hand = col_factor(),
  birthdate = col_date("%Y%m%d"),
  country = col_factor()
))
