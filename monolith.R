####################################################################
# @file: monolith.r
#   This is all of the r scripts used for this project collected 
# into a single script.
#
# @author: Max Vestrand
#




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

############################################
# Utility functions
#


# Creates a new table for storing results
create_results_table <- function() {
  return (list(acc=data.frame(method=character(), accuracy=double()), pred=list() ))
}

# Updates or creates a new row in the results table
update_results_table <- function(results_table, method_name, pred, accuracy) {
  # Add the predicted values
  results_table$pred[method_name] <- list(pred)
  
  
  results_table$acc <- results_table$acc %>%
    filter(method != method_name) %>%
    rbind(data.table(method=method_name, accuracy=accuracy))
  return (results_table)
}



############################################
# Data Exploration
#
#==========================================
# Tidying - Look at the raw data tables
#   to find edge-cases and incorrect values
#==========================================

# Matches without a numeric score
raw_matches %>% 
  filter(!str_detect(score, "[0-9]")) %>%
  group_by(score) %>%
  summarize(n=n())

# Matches counted as retired
raw_matches %>%
  filter(str_detect(score, regex("ret", ignore_case=TRUE))) %>%
  nrow()

# Look at surface values
raw_matches %>%
  group_by(surface) %>%
  summarize(n=n())

# Look at best_of values
raw_matches %>%
  group_by(best_of) %>%
  summarize(n=n())

# Look at na's per column
map(raw_matches, ~sum(is.na(.)))

# Find all rows with any na's
na_matches <- raw_matches %>%
  filter(surface %in% c('None', NA)
         | is.na(w_ace+w_df+w_svpt+w_1stIn+w_1stWon+w_2ndWon+w_SvGms+w_bpSaved+w_bpFaced
                 +l_ace+l_df+l_svpt+l_1stIn+l_1stWon+l_2ndWon+l_SvGms+l_bpSaved+l_bpFaced))

# NA's by year
na_matches %>%
  mutate(year=year(tourney_date)) %>%
  group_by(year) %>%
  summarize(n=n())

# NA's by tourney name
na_matches %>%
  group_by(tourney_name) %>%
  summarize(n=n()) %>%
  arrange(-n)

# # Unparsable scores
# score_regex <- regex(paste("^", strrep("(([0-9]+)-([0-9]+)(\\([0-9]+\\))?)?\\s*",5), "$", sep=""))
# unparsed_matches <- raw_matches %>%
#   mutate(surface=factor(surface),
#          round=factor(round)) %>%
#   filter(!surface %in% c('None', NA)
#          & !is.na(w_ace+w_df+w_svpt+w_1stIn+w_1stWon+w_2ndWon+w_SvGms+w_bpSaved+w_bpFaced
#                   +l_ace+l_df+l_svpt+l_1stIn+l_1stWon+l_2ndWon+l_SvGms+l_bpSaved+l_bpFaced)
#          & str_detect(score, "[0-9]")) %>%
#   mutate(retire = str_detect(score, regex("ret", ignore_case=TRUE))) %>%
#   mutate(score = trim(str_replace(score, regex("ret", ignore_case=TRUE), ""))) %>%
#   filter(!str_detect(score, score_regex))


# Tabulate player stats
player_win_counts <- function(match_data) {
  valid <- match_data %>% filter(str_detect(score, "[0-9]"))
  
  # Count meaningful wins and loses
  win_counts <- valid %>%
    group_by(winner_id) %>%
    summarize(wins = n()) %>%
    rename(player_id = 1)
  lose_counts <- valid %>%
    group_by(loser_id) %>%
    summarize(loses = n()) %>%
    rename(player_id = 1)
  
  total_counts <- full_join(win_counts, lose_counts, by='player_id') %>%
    replace_na(list(wins=0, loses=0, retired_on=0, retires=0, walkovers=0)) %>%
    mutate(matches = wins+loses)
  
}


player_stats <- player_win_counts(raw_matches)

# Players with n matches
plot_n_matches <- player_stats %>% ggplot(aes(matches)) +
  geom_histogram(bins=30, color='black') +
  scale_x_continuous(trans='log2') +
  ggtitle('Players by Number of Matches')

# Players by Win Percentage
plot_win_percent <- player_stats %>% 
  mutate(win_percent = 100*wins/matches) %>% 
  ggplot(aes(win_percent)) +
  geom_histogram(bins=30, color='black') +
  ggtitle('Players by Win%')

# Players by Win-Loss Ratio
lambda <- 0
plot_win_ratio <- player_stats %>% 
  mutate(win_loss = (wins+lambda)/(loses+lambda)) %>% 
  ggplot(aes(win_loss)) +
  geom_histogram(bins=30, color='black') +
  scale_x_continuous(trans='log2') +
  ggtitle('Players by Win-Loss Ratio')

#==================================================
# Examine which features are important to winning
#==================================================

matches_neutral <- neutralize_matches(atp_matches)

# Plot of the base features given
plot_base_features <- matches_neutral %>% gather(key="stat", value="value", p_ace:o_bpFaced) %>%
  ggplot(aes(value, fill=win))+
  geom_density(aes(y=..count..),alpha=0.2)+
  facet_wrap(~stat, scales="free")

# Plot some derived features
derived_stats <- matches_neutral %>%
  mutate(p_aceRate = p_ace / p_svpt,
         p_dfRate = p_df / p_svpt,
         p_1stInRate = p_1stIn / p_svpt,
         p_1stWonRate = p_1stWon / p_1stIn,
         p_2ndWonRate = p_2ndWon / (p_svpt - p_1stIn),
         p_bpSavedRate = p_bpSaved / p_bpFaced,
         p_ret1stWonRate = (o_1stIn - o_1stWon) / o_1stIn,
         p_ret2ndWonRate = (o_svpt - o_1stIn - o_2ndWon) / (o_svpt - o_1stIn),
         p_bpConvRate = (o_bpFaced - o_bpSaved) / o_bpFaced,
         p_svptWonRate = (p_1stWon + p_2ndWon) / p_svpt,
         p_retptWonRate = (o_svpt - o_1stWon - o_2ndWon) / o_svpt,
         p_ptWonRate = (p_1stWon + p_2ndWon + o_svpt - o_1stWon - o_2ndWon)/(p_svpt + o_svpt))

plot_derived_features <- derived_stats %>% gather(key="stat", value="value", p_aceRate:p_ptWonRate) %>%
  ggplot(aes(value, fill=win))+
  geom_density(aes(y=..count..),alpha=0.2)+
  xlim(0,1) +
  facet_wrap(~stat, scales="fixed") +
  ggtitle('Player Statistics in Won vs. Lost Games')


# Fraction of games won with fewer points
won_games <- derived_stats %>%
  filter(win=="Win")
mean(won_games$p_ptWonRate < 0.5)

# Handedness values
atp_players %>%
  group_by(hand) %>%
  summarize(n=n())

# Handedness values of observed players
tibble(player_id = known_final_players) %>%
  left_join(atp_players, "player_id") %>%
  group_by(hand) %>%
  summarize(n=n())



############################################
# Models
#

set.seed(11, sample.kind="Rounding")

res <- create_results_table()



#======================================
# Random Guessing
#======================================

guess_p1 <- sample(c(TRUE, FALSE), nrow(matches_final_test), replace=TRUE)

pred <- matches_final_test %>%
  mutate(pred=ifelse(guess_p1, player1_id, player2_id)) %>%
  .$pred

res <- update_results_table(res, "Random Guessing", pred, mean(pred==matches_final_test$winner_id))


#====================================
# Predict based on win rate
#====================================

# Count wins and loses
win_counts <- matches_final_train %>%
  group_by(winner_id) %>%
  summarize(wins = n()) %>%
  rename(player_id = 1)
lose_counts <- matches_final_train %>%
  group_by(loser_id) %>%
  summarize(loses = n()) %>%
  rename(player_id = 1)

win_rates <- full_join(win_counts, lose_counts, by='player_id') %>%
  replace_na(list(wins=0, loses=0)) %>%
  mutate(win_rate = wins/(wins+loses)) %>%
  select(player_id, win_rate)

# Predict players with the highest win rate
pred <- matches_final_test %>%
  left_join(win_rates, by=c("player1_id"="player_id")) %>%
  rename(p1_win_rate = "win_rate") %>%
  left_join(win_rates, by=c("player2_id"="player_id")) %>%
  rename(p2_win_rate = "win_rate") %>%
  mutate(pred=ifelse(p1_win_rate >= p2_win_rate, player1_id, player2_id)) %>%
  .$pred

res <- update_results_table(res, "Highest Win Rate", pred, mean(pred==matches_final_test$winner_id))


#=======================================
# Predict based on avg points won rate
#=======================================

neutral_train <- neutralize_matches(matches_final_train)

avg_points <- neutral_train %>%
  mutate(p_ptWonRate = (p_1stWon + p_2ndWon + o_svpt - o_1stWon - o_2ndWon)/(p_svpt + o_svpt)) %>%
  group_by(player_id) %>%
  summarize(ptWonRate = mean(p_ptWonRate))

pred <- matches_final_test %>%
  left_join(avg_points, by=c("player1_id"="player_id")) %>%
  rename(p1_ptWonRate = "ptWonRate") %>%
  left_join(avg_points, by=c("player2_id"="player_id")) %>%
  rename(p2_ptWonRate = "ptWonRate") %>%
  mutate(pred=ifelse(p1_ptWonRate >= p2_ptWonRate, player1_id, player2_id)) %>%
  .$pred

res <- update_results_table(res, "Highest Avg Point Won Rate", pred, mean(pred==matches_final_test$winner_id))


#===================================
# Elo Rating system
#===================================
elo_expected <- function(r_p, r_o) {
  1 / (1 + 10^((r_o - r_p)/400))
}
elo_new_rating <- function(r_p, k, actual, expected) {
  r_p + k * (actual - expected)
}

elo_rating <- function(k, r_init, train_set) {
  player_ids <- train_set %>%
    gather(key="player_type", value="player_id", winner_id, loser_id) %>%
    distinct(player_id) %>%
    .$player_id
  
  # Initialize player ratings
  player_ratings <- as.list(rep(r_init, times=length(player_ids)))
  names(player_ratings) <- as.character(player_ids)
  
  # Compute ratings of each player from training set
  for (row in 1:nrow(train_set)) {
    p1 <- train_set[row, "winner_id"]
    p2 <- train_set[row, "loser_id"]
    r1 <- player_ratings[[as.character(p1)]]
    r2 <- player_ratings[[as.character(p2)]]
    e1 <- elo_expected(r1, r2)
    e2 <- 1 - e1
    player_ratings[[as.character(p1)]] <- elo_new_rating(r1, k, 1, e1)
    player_ratings[[as.character(p2)]] <- elo_new_rating(r2, k, 0, e2)
  }
  
  tibble(player_id=names(player_ratings), rating=unlist(player_ratings)) %>%
    mutate(player_id=as.integer(player_id))
}

elo_rating_predict <- function(player_ratings, test_set) {
  
  # Predict player with highest rating
  pred <- test_set %>%
    left_join(player_ratings, by=c("player1_id"="player_id")) %>%
    rename(p1_r = "rating") %>%
    left_join(player_ratings, by=c("player2_id"="player_id")) %>%
    rename(p2_r = "rating") %>%
    mutate(pred=ifelse(p1_r >= p2_r, player1_id, player2_id)) %>%
    .$pred
  
}

r_init <- 1500
k_values <- seq(20,36, by=8)
accuracies <- sapply(k_values, FUN=function(k){
  print(k)
  ratings <- elo_rating(k, r_init, matches_train)
  pred <- elo_rating_predict(ratings, matches_test)
  mean(pred==matches_test$winner_id)
})
best_k <- k_values[which.max(accuracies)]

ratings <- elo_rating(best_k, r_init, matches_final_train)
pred <- elo_rating_predict(ratings, matches_final_test)

plot(k_values, accuracies)

res <- update_results_table(res, "Elo Rating System", pred, mean(pred==matches_final_test$winner_id))


#==========================================
# Elo rating with weighted points won rate
#==========================================
elo_rating_weighted_score <- function(k, r_init, w, train_set) {
  player_ids <- train_set %>%
    gather(key="player_type", value="player_id", winner_id, loser_id) %>%
    distinct(player_id) %>%
    .$player_id
  
  # Initialize player ratings
  player_ratings <- as.list(rep(r_init, times=length(player_ids)))
  names(player_ratings) <- as.character(player_ids)
  
  # Compute ratings of each player from training set
  for (row in 1:nrow(train_set)) {
    p1 <- train_set[row, "winner_id"]
    p2 <- train_set[row, "loser_id"]
    r1 <- player_ratings[[as.character(p1)]]
    r2 <- player_ratings[[as.character(p2)]]
    e1 <- elo_expected(r1, r2)
    e2 <- 1 - e1
    p1_ptWonRate <- train_set[row, "ptWonRate"]
    p2_ptWonRate <- 1 - p1_ptWonRate
    player_ratings[[as.character(p1)]] <- elo_new_rating(r1, k, (1-w)+w*p1_ptWonRate, e1)
    player_ratings[[as.character(p2)]] <- elo_new_rating(r2, k, w*p2_ptWonRate, e2)
    
  }
  
  player_final_ratings <- tibble(player_id=names(player_ratings), rating=unlist(player_ratings)) %>%
    mutate(player_id=as.integer(player_id))
}

# w - Weight given to points won rate
elo_rating_weighted_score_predict <- function(player_ratings, test_set) {
  # Predict player with highest rating
  pred <- test_set %>%
    left_join(player_ratings, by=c("player1_id"="player_id")) %>%
    rename(p1_r = "rating") %>%
    left_join(player_ratings, by=c("player2_id"="player_id")) %>%
    rename(p2_r = "rating") %>%
    mutate(pred=ifelse(p1_r >= p2_r, player1_id, player2_id)) %>%
    .$pred
  
}

# Try different weights for score
r_init <- 1500
k <- best_k
w_values <- seq(0,1, by=.1)
best_w <- 1

# # Very slow
# accuracies <- sapply(w_values, FUN=function(w){
#   print(w)
#   ratings <- elo_rating_weighted_score(k, r_init, w, matches_train)
#   pred <- elo_rating_weighted_score_predict(ratings, matches_test)
#   mean(pred==matches_test$winner_id)
# })
# best_w <- w_values[which.max(accuracies)]


# Compute ratings on the entire training set
ratings <- elo_rating_weighted_score(k, r_init, best_w, matches_final_train)
pred <- elo_rating_weighted_score_predict(ratings, matches_final_test)


res <- update_results_table(res, "Elo Rating System w/ Weighted Points Won Rate", pred, mean(pred==matches_final_test$winner_id))


#================================================
# Linear regression model to predict score rate
#================================================

# Compute player specific statistics
get_player_stats <- function(player_data, player_ratings, match_data) {
  neutral_matches <- neutralize_matches(match_data)
  player_stats <- neutral_matches %>%
    mutate(p_aceRate = p_ace / p_svpt,
           p_dfRate = p_df / p_svpt,
           p_1stInRate = p_1stIn / p_svpt,
           p_1stWonRate = p_1stWon / p_1stIn,
           p_2ndWonRate = p_2ndWon / (p_svpt - p_1stIn),
           p_bpSavedRate = p_bpSaved / p_bpFaced,
           p_ret1stWonRate = (o_1stIn - o_1stWon) / o_1stIn,
           p_ret2ndWonRate = (o_svpt - o_1stIn - o_2ndWon) / (o_svpt - o_1stIn),
           p_bpConvRate = (o_bpFaced - o_bpSaved) / o_bpFaced,
           p_svptWonRate = (p_1stWon + p_2ndWon) / p_svpt,
           p_retptWonRate = (o_svpt - o_1stWon - o_2ndWon) / o_svpt,
           p_ptWonRate = (p_1stWon + p_2ndWon + o_svpt - o_1stWon - o_2ndWon)/(p_svpt + o_svpt)) %>%
    group_by(player_id) %>%
    summarize(aceRate = mean(p_aceRate),
              dfRate = mean(p_dfRate),
              sv1stInRate = mean(p_1stInRate),
              sv1stWonRate = mean(p_1stWonRate),
              sv2ndWonRate = mean(p_2ndWonRate),
              bpSavedRate = mean(p_bpSavedRate),
              ret1stWonRate = mean(p_ret1stWonRate),
              ret2ndWonRate = mean(p_ret2ndWonRate),
              bpConvRate = mean(p_bpConvRate),
              svptWonRate = mean(p_svptWonRate),
              retptWonRate = mean(p_retptWonRate),
              ptWonRate = mean(p_ptWonRate)) %>%
    mutate(bpSavedRate = ifelse(is.nan(bpSavedRate), 0, bpSavedRate),
           bpConvRate = ifelse(is.nan(bpConvRate), 0, bpConvRate)) %>%
    left_join(player_ratings, by="player_id") %>% 
    left_join(player_data, by='player_id') %>%
    select(-birthdate) %>%
    mutate(hand = ifelse(hand=="L", 1, -1)) %>% # Assume unknowns are right-handed since it is more common
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate_all(~replace(., is.nan(.), 0))
  
}

# Compute stats of a player relative to their opponent
get_match_stats <- function(player_stats, match_data) {
  match_data %>%
    left_join(player_stats, by=c("player1_id" = "player_id")) %>%
    left_join(player_stats, by=c("player2_id" = "player_id")) %>%
    mutate(aceDiff = aceRate.x - aceRate.y,
           dfDiff = dfRate.x - dfRate.y,
           ratingDiff = rating.x - rating.y,
           off1st = sv1stWonRate.x - ret1stWonRate.y,
           off2nd = sv2ndWonRate.x - ret2ndWonRate.y,
           offBp = bpConvRate.x - bpSavedRate.y,
           offpt = svptWonRate.x - retptWonRate.y,
           def1st = ret1stWonRate.x - sv1stWonRate.y,
           def2nd = ret2ndWonRate.x - sv2ndWonRate.y,
           defBp = bpSavedRate.x - bpConvRate.y,
           defpt = retptWonRate.x - svptWonRate.y,
           hand = hand.x - hand.y)
}

simplify_match_stats <- function(match_stats) {
  match_stats %>%
    select(win,
           aceDiff,
           dfDiff,
           ratingDiff,
           off1st,
           off2nd,
           offBp,
           offpt,
           def1st,
           def2nd,
           defBp,
           defpt,
           hand)
}

player_stats <- get_player_stats(atp_players, ratings, matches_final_train)


match_stats <- get_match_stats(player_stats, shuffle_matches(matches_final_train))
match_stats <- simplify_match_stats(match_stats)

test_match_stats <- simplify_match_stats(get_match_stats(player_stats, matches_final_test))

fit <- train(win ~ ., method = "glm", data = match_stats)
pred <- predict(fit, test_match_stats)

res <- update_results_table(res, "GLM", pred, mean(pred==matches_final_test$win))



#==========================
# Explore match stats
#==========================
plot_factors_vs_rating <- match_stats %>%
  gather(key="stat", value="value", off1st:hand) %>%
  ggplot(aes(x=value, y=ratingDiff, color=win)) +
  geom_point(alpha=0.2) +
  facet_wrap(~stat, scales="fixed") +
  ggtitle('Match Factors vs. Rating Difference')

