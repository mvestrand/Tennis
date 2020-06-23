########################################################
# Explore the dataset
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
  
# Unparsable scores
score_regex <- regex(paste("^", strrep("(([0-9]+)-([0-9]+)(\\([0-9]+\\))?)?\\s*",5), "$", sep=""))
unparsed_matches <- raw_matches %>%
  mutate(surface=factor(surface),
         round=factor(round)) %>%
  filter(!surface %in% c('None', NA)
         & !is.na(w_ace+w_df+w_svpt+w_1stIn+w_1stWon+w_2ndWon+w_SvGms+w_bpSaved+w_bpFaced
                  +l_ace+l_df+l_svpt+l_1stIn+l_1stWon+l_2ndWon+l_SvGms+l_bpSaved+l_bpFaced)
         & str_detect(score, "[0-9]")) %>%
  mutate(retire = str_detect(score, regex("ret", ignore_case=TRUE))) %>%
  mutate(score = trim(str_replace(score, regex("ret", ignore_case=TRUE), ""))) %>%
  filter(!str_detect(score, score_regex))

  
# Tabulate player stats
get_player_stats <- function(match_data) {
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
    left_join(retired_on_counts, by='player_id') %>%
    left_join(retire_counts, by='player_id') %>%
    left_join(walkover_counts, by='player_id') %>%
    replace_na(list(wins=0, loses=0, retired_on=0, retires=0, walkovers=0)) %>%
    mutate(matches = wins+loses)
  
}


player_stats <- get_player_stats(raw_matches)

# Players with n matches
player_stats %>% ggplot(aes(matches)) +
  geom_histogram(bins=30, color='black') +
  scale_x_continuous(trans='log2') +
  ggtitle('Players by Number of Matches')

# Players by Win Percentage
player_stats %>% 
  mutate(win_percent = 100*wins/matches) %>% 
  ggplot(aes(win_percent)) +
  geom_histogram(bins=30, color='black') +
  ggtitle('Players by Win Percentage')

# Players by Win-Loss Ratio
lambda <- 0
player_stats %>% 
  mutate(win_loss = (wins+lambda)/(loses+lambda)) %>% 
  ggplot(aes(win_loss)) +
  geom_histogram(bins=30, color='black') +
  scale_x_continuous(trans='log2') +
  ggtitle('Players by Win-Loss Ratio')
