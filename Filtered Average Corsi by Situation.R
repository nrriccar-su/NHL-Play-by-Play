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
# Calculate the averages for each relevant column
averages <- filtered_data %>%
  summarise(
    avg_away_at_fight = mean(away_corsi_at_fight, na.rm = TRUE),
    avg_home_at_fight = mean(home_corsi_at_fight, na.rm = TRUE),
    avg_away_after_2 = mean(away_corsi_after_2, na.rm = TRUE),
    avg_home_after_2 = mean(home_corsi_after_2, na.rm = TRUE),
    avg_away_after_5 = mean(away_corsi_after_5, na.rm = TRUE),
    avg_home_after_5 = mean(home_corsi_after_5, na.rm = TRUE),
    avg_away_after_10 = mean(away_corsi_after_10, na.rm = TRUE),
    avg_home_after_10 = mean(home_corsi_after_10, na.rm = TRUE),
    avg_away_until_eop = mean(away_corsi_until_eop, na.rm = TRUE),
    avg_home_until_eop = mean(home_corsi_until_eop, na.rm = TRUE),
    avg_away_until_eog = mean(away_corsi_until_eog, na.rm = TRUE),
    avg_home_until_eog = mean(home_corsi_until_eog, na.rm = TRUE),
    avg_away_until_next_fight = mean(away_corsi_until_next_fight, na.rm = TRUE),
    avg_home_until_next_fight = mean(home_corsi_until_next_fight, na.rm = TRUE)
  )
# Reshape the data for easier plotting
plot_data <- data.frame(
  Situation = c("At Fight", "After 2", "After 5", "After 10", "Until EOP", "Until EOG", "Until Next Fight"),
  Away = c(averages$avg_away_at_fight, averages$avg_away_after_2, averages$avg_away_after_5,
           averages$avg_away_after_10, averages$avg_away_until_eop, averages$avg_away_until_eog, 
           averages$avg_away_until_next_fight),
  Home = c(averages$avg_home_at_fight, averages$avg_home_after_2, averages$avg_home_after_5,
           averages$avg_home_after_10, averages$avg_home_until_eop, averages$avg_home_until_eog, 
           averages$avg_home_until_next_fight)
)

# Reshape for ggplot
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(Away, Home), names_to = "Team", values_to = "Average_Corsi")
# Set the Situation column as a factor with the desired order
plot_data_long$Situation <- factor(plot_data_long$Situation, 
                                   levels = c("At Fight", "After 2", "After 5", "After 10", 
                                              "Until EOP", "Until Next Fight", "Until EOG"))

# Create the line chart
ggplot(plot_data_long, aes(x = Situation, y = Average_Corsi, color = Team, group = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Average Corsi in Different Situations",
       x = "Situation",
       y = "Average Corsi",
       color = "Team") +
  theme(plot.title = element_text(hjust = 0.5))

#REMAKE UP UNTIL EOP
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
# Calculate the averages for each relevant column
averages <- filtered_data %>%
  summarise(
    avg_away_at_fight = mean(away_corsi_at_fight, na.rm = TRUE),
    avg_home_at_fight = mean(home_corsi_at_fight, na.rm = TRUE),
    avg_away_after_2 = mean(away_corsi_after_2, na.rm = TRUE),
    avg_home_after_2 = mean(home_corsi_after_2, na.rm = TRUE),
    avg_away_after_5 = mean(away_corsi_after_5, na.rm = TRUE),
    avg_home_after_5 = mean(home_corsi_after_5, na.rm = TRUE),
    avg_away_after_10 = mean(away_corsi_after_10, na.rm = TRUE),
    avg_home_after_10 = mean(home_corsi_after_10, na.rm = TRUE),
    avg_away_until_eop = mean(away_corsi_until_eop, na.rm = TRUE),
    avg_home_until_eop = mean(home_corsi_until_eop, na.rm = TRUE),
    avg_away_until_eog = mean(away_corsi_until_eog, na.rm = TRUE),
    avg_home_until_eog = mean(home_corsi_until_eog, na.rm = TRUE),
    avg_away_until_next_fight = mean(away_corsi_until_next_fight, na.rm = TRUE),
    avg_home_until_next_fight = mean(home_corsi_until_next_fight, na.rm = TRUE)
  )
# Reshape the data for easier plotting
plot_data <- data.frame(
  Situation = c("At Fight", "After 2", "After 5", "After 10", "Until EOP"),
  Away = c(averages$avg_away_at_fight, averages$avg_away_after_2, averages$avg_away_after_5,
           averages$avg_away_after_10, averages$avg_away_until_eop),
  Home = c(averages$avg_home_at_fight, averages$avg_home_after_2, averages$avg_home_after_5,
           averages$avg_home_after_10, averages$avg_home_until_eop)
)

# Reshape for ggplot
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(Away, Home), names_to = "Team", values_to = "Average_Corsi")
# Set the Situation column as a factor with the desired order
plot_data_long$Situation <- factor(plot_data_long$Situation, 
                                   levels = c("At Fight", "After 2", "After 5", "After 10", 
                                              "Until EOP"))

# Create the line chart
ggplot(plot_data_long, aes(x = Situation, y = Average_Corsi, color = Team, group = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Average Corsi in Different Situations",
       x = "Situation",
       y = "Average Corsi",
       color = "Team") +
  theme(plot.title = element_text(hjust = 0.5))

