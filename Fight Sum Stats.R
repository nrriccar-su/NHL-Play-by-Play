# Group by game_id and find the max number of fights per game in nhl_data
fights_per_game <- nhl_data %>%
  group_by(game_id) %>%
  summarise(fights = max(game_fight_number, na.rm = TRUE)) %>%
  ungroup()

# Find game_ids in pbp_data that are not in nhl_data (these have zero fights)
no_fights_games <- pbp_data %>%
  filter(!game_id %in% nhl_data$game_id) %>%
  group_by(game_id) %>%
  summarise(fights = 0) %>%  # Assign zero fights
  ungroup()

# Combine no fights games and fights_per_game
combined_data <- bind_rows(fights_per_game, no_fights_games)

# Summary statistics
summary_stats <- combined_data %>%
  summarise(
    Min = min(fights),
    Max = max(fights),
    Mean = mean(fights),
    Median = median(fights),
    Q1 = quantile(fights, 0.25),
    Q3 = quantile(fights, 0.75)
  )

# Print summary stats to console
print(summary_stats)

# Group by game_id and find the max number of fights per game in nhl_data
fights_per_game <- nhl_data %>%
  group_by(game_id) %>%
  summarise(fights = max(game_fight_number, na.rm = TRUE)) %>%
  ungroup()

# Find game_ids in pbp_data that are not in nhl_data (these have zero fights)
no_fights_games <- pbp_data %>%
  filter(!game_id %in% nhl_data$game_id) %>%
  group_by(game_id) %>%
  summarise(fights = 0) %>%  # Assign zero fights
  ungroup()

# Combine no fights games and fights_per_game
combined_data <- bind_rows(fights_per_game, no_fights_games)

# Summary statistics
summary_stats <- combined_data %>%
  summarise(
    Min = min(fights),
    Max = max(fights),
    Mean = mean(fights),
    Median = median(fights),
    Q1 = quantile(fights, 0.25),
    Q3 = quantile(fights, 0.75)
  )

# Print summary stats to console
print(summary_stats)

# Histogram for number of fights per game
ggplot(combined_data, aes(x = fights)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "darkgreen", boundary = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of Fights per Game",
       x = "Number of Fights",
       y = "Count of Games") +
  theme(plot.title = element_text(hjust = 0.5))



