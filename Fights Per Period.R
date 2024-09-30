# Load necessary library
library(dplyr)

# Read the CSV file
nhl_data <- read.csv("NHL_pbp_Fight_aggs3.csv")

# Filter the dataset
filtered_data <- nhl_data %>%
  filter(game_time_of_fight < 3300 & 
           abs(away_score_at_fight - home_score_at_fight) %in% c(0, 1, 2))

# View the filtered data
print(filtered_data)
# Create the frequency table with counts
frequency_table <- filtered_data %>%
  summarise(
    away_greater_than_home = sum(away_score_at_fight > home_score_at_fight, na.rm = TRUE),
    home_greater_than_away = sum(home_score_at_fight > away_score_at_fight, na.rm = TRUE),
    scores_equal = sum(away_score_at_fight == home_score_at_fight, na.rm = TRUE),
    period_1 = sum(game_period_number == 1, na.rm = TRUE),
    period_2 = sum(game_period_number == 2, na.rm = TRUE),
    period_3 = sum(game_period_number == 3, na.rm = TRUE)
  )

# Add a row for percentages
frequency_table <- frequency_table %>%
  mutate(
    total_scores = away_greater_than_home + home_greater_than_away + scores_equal,
    total_periods = period_1 + period_2 + period_3,
    away_greater_than_home_pct = (away_greater_than_home / total_scores) * 100,
    home_greater_than_away_pct = (home_greater_than_away / total_scores) * 100,
    scores_equal_pct = (scores_equal / total_scores) * 100,
    period_1_pct = (period_1 / total_periods) * 100,
    period_2_pct = (period_2 / total_periods) * 100,
    period_3_pct = (period_3 / total_periods) * 100
  ) %>%
  select(-total_scores, -total_periods)  # Remove the intermediate totals if not needed

# View the frequency table with percentages
print(frequency_table)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a data frame for the score comparison frequencies
score_comparison <- filtered_data %>%
  summarise(
    Category = c("Away > Home", "Home > Away", "Scores Equal"),
    Count = c(sum(away_score_at_fight > home_score_at_fight, na.rm = TRUE),
              sum(home_score_at_fight > away_score_at_fight, na.rm = TRUE),
              sum(away_score_at_fight == home_score_at_fight, na.rm = TRUE))
  ) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Bar plot for score comparison
ggplot(score_comparison, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Score Comparison at Fight",
       x = "Category",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

# Create a data frame for the game period frequencies
game_periods <- nhl_data %>%
  summarise(
    Period = c("Period 1", "Period 2", "Period 3"),
    Count = c(sum(game_period_number == 1, na.rm = TRUE),
              sum(game_period_number == 2, na.rm = TRUE),
              sum(game_period_number == 3, na.rm = TRUE))
  ) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Bar plot for game periods
ggplot(game_periods, aes(x = Period, y = Count, fill = Period)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Game Period Distribution",
       x = "Period",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

