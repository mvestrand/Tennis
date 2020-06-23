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
# Guess based on win rate
#====================================

# Count meaningful wins and loses
win_counts <- matches_train %>%
  group_by(winner_id) %>%
  summarize(wins = n()) %>%
  rename(player_id = 1)
lose_counts <- matches_train %>%
  group_by(loser_id) %>%
  summarize(loses = n()) %>%
  rename(player_id = 1)

total_counts <- full_join(win_counts, lose_counts, by='player_id') %>%
  replace_na(list(wins=0, loses=0)) %>%
  mutate(win = wins+loses) %>%
  
