# Create a data frame for the score difference frequencies
score_difference <- filtered_data %>%
  summarise(
    Category = c("Away +2", "Away +1", "Equal", "Home +1", "Home +2"),
    Count = c(sum(away_score_at_fight - home_score_at_fight == 2, na.rm = TRUE),
              sum(away_score_at_fight - home_score_at_fight == 1, na.rm = TRUE),
              sum(away_score_at_fight == home_score_at_fight, na.rm = TRUE),
              sum(home_score_at_fight - away_score_at_fight == 1, na.rm = TRUE),
              sum(home_score_at_fight - away_score_at_fight == 2, na.rm = TRUE))
  ) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Bar plot for score difference comparison
ggplot(score_difference, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Score Difference at Fight",
       x = "Score Difference Category",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))
