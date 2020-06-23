########################################################
# Try different models and evaluate their effectiveness
#

set.seed(11, sample.kind="Rounding")

res <- create_results_table()



#======================================
# Random Guessing
#======================================

guess_p1 <- sample(c(TRUE, FALSE), nrow(matches_test), replace=TRUE)

pred <- matches_test %>%
  mutate(pred=ifelse(guess_p1, player1_id, player2_id)) %>%
  .$pred

res <- update_results_table(res, "Random Guessing", pred, mean(pred==matches_test$winner_id))


#====================================
# Predict based on win rate
#====================================

# Count wins and loses
win_counts <- matches_train %>%
  group_by(winner_id) %>%
  summarize(wins = n()) %>%
  rename(player_id = 1)
lose_counts <- matches_train %>%
  group_by(loser_id) %>%
  summarize(loses = n()) %>%
  rename(player_id = 1)

win_rates <- full_join(win_counts, lose_counts, by='player_id') %>%
  replace_na(list(wins=0, loses=0)) %>%
  mutate(win_rate = wins/(wins+loses)) %>%
  select(player_id, win_rate)

# Predict players with the highest win rate
pred <- matches_test %>%
  left_join(win_rates, by=c("player1_id"="player_id")) %>%
  rename(p1_win_rate = "win_rate") %>%
  left_join(win_rates, by=c("player2_id"="player_id")) %>%
  rename(p2_win_rate = "win_rate") %>%
  mutate(pred=ifelse(p1_win_rate >= p2_win_rate, player1_id, player2_id)) %>%
  .$pred
  
res <- update_results_table(res, "Highest Win Rate", pred, mean(pred==matches_test$winner_id))

