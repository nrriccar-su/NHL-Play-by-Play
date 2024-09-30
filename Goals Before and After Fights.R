# Load necessary libraries
library(dplyr)

# Read the CSV file
pbp_data <- read.csv("NHL_pbp_w_Corsi-Fenwick.csv")

# Step 1: Get game_ids with at least one "fighting"
game_ids_with_fights <- pbp_data %>%
  filter(description_key == "fighting") %>%
  pull(game_id) %>%
  unique()

# Step 2: Keep all rows for those game_ids
filtered_data <- pbp_data %>%
  filter(game_id %in% game_ids_with_fights)

# Step 3: Calculate average goals before and after fights for each game_id
average_goals <- filtered_data %>%
  group_by(game_id) %>%
  # Add a column to identify the first row index where the fight occurs
  mutate(fight_row_index = which(description_key == "fighting")[1]) %>%
  summarise(
    # Ensure calculations are only made if the fight_row_index is valid
    avg_goals_before = ifelse(!is.na(fight_row_index) && fight_row_index > 1, 
                              mean(away_goal_cumsum[1:(fight_row_index - 1)], na.rm = TRUE) + 
                                mean(home_goal_cumsum[1:(fight_row_index - 1)], na.rm = TRUE), 
                              NA),
    avg_goals_after = ifelse(!is.na(fight_row_index) && fight_row_index < n(), 
                             mean(away_goal_cumsum[(fight_row_index + 1):n()], na.rm = TRUE) + 
                               mean(home_goal_cumsum[(fight_row_index + 1):n()], na.rm = TRUE), 
                             NA),
    NA),
.groups = 'drop'  # This drops the grouping afterwards
) %>%
  # Filter out rows with NA values (indicating no goals before or after fights)
  filter(!is.na(avg_goals_before) & !is.na(avg_goals_after))

# Step 4: Calculate overall averages only for valid game_ids
overall_averages <- average_goals %>%
  summarise(
    total_avg_goals_before = mean(avg_goals_before, na.rm = TRUE),
    total_avg_goals_after = mean(avg_goals_after, na.rm = TRUE)
  )

# Display the overall averages
print(overall_averages)
