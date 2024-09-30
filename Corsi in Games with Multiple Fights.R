# Load necessary library
library(dplyr)
library(ggplot2)

# Filter dataset to only include rows where game_id appears multiple times
filtered_data_multiple_game_id <- filtered_data %>%
  group_by(game_id) %>%
  filter(n() > 1) %>%
  ungroup()

# Calculate the averages for the relevant columns
averages_multiple_game_id <- filtered_data_multiple_game_id %>%
  summarise(
    avg_away_at_fight = mean(away_corsi_at_fight, na.rm = TRUE),
    avg_home_at_fight = mean(home_corsi_at_fight, na.rm = TRUE),
    avg_away_until_next_fight = mean(away_corsi_until_next_fight, na.rm = TRUE),
    avg_home_until_next_fight = mean(home_corsi_until_next_fight, na.rm = TRUE),
    avg_away_until_eog = mean(away_corsi_until_eog, na.rm = TRUE),
    avg_home_until_eog = mean(home_corsi_until_eog, na.rm = TRUE)
  )

# Reshape the data for easier plotting
plot_data_multiple_game_id <- data.frame(
  Situation = c("At Fight", "Until Next Fight", "Until EOG"),
  Away = c(averages_multiple_game_id$avg_away_at_fight, 
           averages_multiple_game_id$avg_away_until_next_fight, 
           averages_multiple_game_id$avg_away_until_eog),
  Home = c(averages_multiple_game_id$avg_home_at_fight, 
           averages_multiple_game_id$avg_home_until_next_fight, 
           averages_multiple_game_id$avg_home_until_eog)
)

# Reshape for ggplot
plot_data_long_multiple_game_id <- plot_data_multiple_game_id %>%
  pivot_longer(cols = c(Away, Home), names_to = "Team", values_to = "Average_Corsi") %>%
  filter(!is.na(Average_Corsi))  # Remove rows with NA values

# Set the Situation column as a factor with the desired order
plot_data_long_multiple_game_id$Situation <- factor(plot_data_long_multiple_game_id$Situation, 
                                                    levels = c("At Fight", "Until Next Fight", "Until EOG"))

# Create the line chart
ggplot(plot_data_long_multiple_game_id, aes(x = Situation, y = Average_Corsi, color = Team, group = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Average Corsi in Specific Situations (Games with Multiple Fights)",
       x = "Situation",
       y = "Average Corsi",
       color = "Team") +
  theme(plot.title = element_text(hjust = 0.5))
