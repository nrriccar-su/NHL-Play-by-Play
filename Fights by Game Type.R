# Load necessary library
library(dplyr)

# Read the CSV files
nhl_data <- read.csv("NHL_pbp_Fight_aggs3.csv")
corsi_fenwick_data <- read.csv("NHL_pbp_w_Corsi-Fenwick.csv")

# Filter the nhl_data as per your earlier code
filtered_data <- nhl_data %>%
  filter(game_time_of_fight < 3300 & 
           abs(away_score_at_fight - home_score_at_fight) %in% c(0, 1, 2))

# Select only the relevant columns from the Corsi-Fenwick dataset
# And remove duplicate game_id entries
corsi_fenwick_data_subset <- corsi_fenwick_data %>%
  select(game_id, away_team_name, home_team_name) %>%
  distinct(game_id, .keep_all = TRUE)

# Perform a left join to add team names to the filtered data (retain only filtered_data rows)
# This will only keep the 700+ rows from filtered_data and append matching team names
joined_filtered_data <- filtered_data %>%
  left_join(corsi_fenwick_data_subset, by = "game_id")

# View the resulting joined data
print(joined_filtered_data)

# Optionally, write the resulting joined data to a new CSV file
write.csv(joined_filtered_data, "filtered_Fight_aggs3_with_team_names.csv", row.names = FALSE)

new_joined_filtered_data <- read.csv("filtered_Fight_aggs3_with_team_names.csv")

# Assuming the necessary libraries are already loaded
library(ggplot2)
library(dplyr)

# Create a new column based on the match criteria
new_joined_filtered_data <- new_joined_filtered_data %>%
  mutate(Match_Category = case_when(
    away_team_division == home_team_division ~ "Division Match",
    away_team_conference == home_team_conference & away_team_division != home_team_division ~ "Conference Match, Different Division",
    away_team_conference != home_team_conference ~ "Conference Mismatch"
  ))

# Create a summary table to calculate the percentage of rows in each category
match_summary <- new_joined_filtered_data %>%
  group_by(Match_Category) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Adjust the factor levels to reorder the bars and update labels
match_summary$Match_Category <- factor(match_summary$Match_Category, 
                                       levels = c("Division Match", 
                                                  "Conference Match, Different Division", 
                                                  "Conference Mismatch"),
                                       labels = c("Division Game", 
                                                  "Conference Game, Different Division", 
                                                  "Different Conference Game"))

# Create the updated bar plot with x-axis labels removed and renamed labels
ggplot(match_summary, aes(x = Match_Category, y = Percentage, fill = Match_Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Percentage of Fights by Game Type",
       x = "Game Type",
       y = "Percentage",
       fill = "Game Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank()) # Remove x-axis labels



