# Load necessary library
library(ggplot2)
library(dplyr)

# Read the CSV file
nhl_data <- read.csv("NHL_pbp_Fight_aggs3.csv")

# Filter the dataset for game_period_number = 3
filtered_period_3 <- nhl_data %>%
  filter(game_period_number == 3)

# Create a new column for the game_time_of_fight category
time_category <- filtered_period_3 %>%
  mutate(Time_Category = if_else(game_time_of_fight < 3300, 
                                 "More than 5 minutes remaining", 
                                 "5 minutes remaining or less"))

# Summarize the counts and calculate percentages
time_summary <- time_category %>%
  group_by(Time_Category) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a bar plot with the updated labels
ggplot(time_summary, aes(x = Time_Category, y = Percentage, fill = Time_Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Percentage of Fights by Game Time in Period 3",
       x = "Game Time Category",
       y = "Percentage",
       fill = "Game Time Category") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank()) # Remove x-axis labels

