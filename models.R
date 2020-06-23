####################################################################
# @file: models.r
#   Tries different models for predicting matches and records results
#
# @author: Max Vestrand
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
k_values <- seq(20,36, by=2)
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
accuracies <- sapply(w_values, FUN=function(w){
  print(w)
  ratings <- elo_rating_weighted_score(k, r_init, w, matches_train)
  pred <- elo_rating_weighted_score_predict(ratings, matches_test)
  mean(pred==matches_test$winner_id)
})
best_w <- w_values[which.max(accuracies)]

# Compute ratings on the entire training set
ratings <- elo_rating_weighted_score(k, r_init, best_w, matches_final_train)
pred <- elo_rating_weighted_score_predict(ratings, matches_final_test)

plot(w_values, accuracies)

plot_w_accuracy <- tibble(w=w_values, accuracy=accuracies) %>%
  ggplot(aes(w, accuracy)) +
  geom_line()


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

