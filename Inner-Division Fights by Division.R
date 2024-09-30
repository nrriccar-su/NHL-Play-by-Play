# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the CSV file
filtered_data <- read.csv("filtered_Fight_aggs3_with_team_names.csv")

# Filter rows where away_team_division is the same as home_team_division
same_division_data <- filtered_data %>%
  filter(away_team_division == home_team_division & 
           away_team_division != "" & 
           home_team_division != "")

# Count the occurrences of each division
division_counts <- same_division_data %>%
  count(away_team_division) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(division_counts, aes(x = away_team_division, y = percentage, fill = away_team_division)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(percentage, 1)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Percentage of Inner-Division Fights by Division",
       x = "Division",
       y = "Percentage of Fights") +
  theme(plot.title = element_text(hjust = 0.5))
