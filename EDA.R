
# Loading Packages --------------------------------------------------------

library(tidyverse)
library(skimr)
library(corrplot)

# Loading Data ------------------------------------------------------------

train_dat <- read_csv(file = "data/processed/training_data.csv")

# Analysis of Missingness -------------------------------------------------

skim_without_charts(train_dat)

ggplot(data = (train_dat %>% filter(is.na(team_favorite_id) == TRUE)), aes(schedule_season)) +
  geom_bar(fill = "blue") +
  labs(
    x = "Season",
    y = "Number of NAs",
    title = "Missingness in Favored Team"
  ) +
  theme_minimal()

humid_plot <- ggplot(data = (train_dat %>% filter(is.na(weather_humidity) == TRUE)), aes(schedule_season)) +
  geom_bar(fill = "blue") +
  labs(
    x = "Season",
    y = "Number of NAs",
    title = "Missingness in Weather Humidity"
  ) +
  theme_minimal()
    
temp_plot <- ggplot(data = (train_dat %>% filter(is.na(weather_temperature) == TRUE)), aes(schedule_season)) +
  geom_bar(fill = "blue") +
  labs(
    x = "Season",
    y = "Number of NAs",
    title = "Missingness in Weather Temperature"
  ) +
  theme_minimal()     

detail_plot <- ggplot(data = (train_dat %>% filter(is.na(weather_detail) == TRUE)), aes(schedule_season)) +
  geom_bar(fill = "blue") +
  labs(
    x = "Season",
    y = "Number of NAs",
    title = "Missingness in Weather Detail"
  ) +
  theme_minimal() 

wind_plot <- ggplot(data = (train_dat %>% filter(is.na(weather_wind_mph) == TRUE)), aes(schedule_season)) +
  geom_bar(fill = "blue") +
  labs(
    x = "Season",
    y = "Number of NAs",
    title = "Missingness in Weather Wind MPH"
  ) +
  theme_minimal()     

cowplot::plot_grid(humid_plot, detail_plot, temp_plot, wind_plot, ncol = 2)

# Corrplot ----------------------------------------------------------------

train_wo_result <- train_dat %>%
  select(-c(score_home, score_away, winning_team, losing_team, home_team_year, away_team_year,
            home_win, home_loss, away_win, away_loss, post_home_team_wins, post_home_team_losses,
            post_away_team_losses, post_away_team_wins, pre_away_team_losses, pre_away_team_wins,
            pre_home_team_losses, pre_home_team_losses, spread_result, score_sum))

train_wo_result %>% select_if(is.numeric) %>%
  filter_all(any_vars(!is.na(.))) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot()

train_dat %>% filter_all(any_vars(!is.na(.))) %>%
  mutate(result = favorite_hit + underdog_hit + push) %>%
  group_by(result) %>% count()

# Investigation of Response Variable --------------------------------------

train_dat %>% filter(!is.na(bet_result_on_favorite)) %>% group_by(schedule_season) %>% 
  summarise(mean = mean(bet_result_on_favorite)) %>%
  ggplot(aes(schedule_season, mean)) +
  geom_point() +
  theme_minimal()

train_dat %>% filter(!is.na(favorite_hit)) %>% 
  summarise(fav_bet_mean_result = mean(bet_result_on_favorite))

train_dat %>% filter(!is.na(favorite_hit)) %>% group_by(home_favored) %>% 
  summarise(fav_bet_mean_result = mean(bet_result_on_favorite))

train_dat %>% filter(!is.na(favorite_hit)) %>% group_by(schedule_playoff) %>%
  summarise(fav_bet_mean_result = mean(bet_result_on_favorite))

train_dat %>% filter(!is.na(favorite_hit)) %>%
  summarise()

train_dat %>% filter(!is.na(bet_result_on_favorite)) %>% group_by(schedule_week) %>%
  summarise(fav_bet_mean_result = mean(bet_result_on_favorite)) %>%
  ggplot(aes(fav_bet_mean_result, schedule_week)) +
  geom_col()

train_dat %>% filter(!is.na(bet_result_on_favorite)) %>% group_by(schedule_week) %>%
  summarise(fav_bet_mean_result = mean(bet_result_on_favorite)) %>%
  ggplot(aes(fav_bet_mean_result, 
             factor(schedule_week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                                              "14", "15", "16", "17", "18", "Wildcard", "Division", "Conference", "Superbowl")))) +
  geom_col(aes(fill = fav_bet_mean_result), show.legend = FALSE) +
  labs(
    x = "Bet Result Average on Favorite",
    y = "Week of Season"
  ) +
  theme_minimal()


# Secondary Stuff ---------------------------------------------------------

train_dat %>% filter(!is.na(over_under_line)) %>% 
  ggplot(aes(over_under_line, score_sum)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  annotate(geom = "text", label = "y = x", x = 65, y = 70, color = "red") +
  labs(
    x = "Over Under for Game",
    y = "Total Points Scored in Game"
  ) +
  theme_minimal()

train_dat %>% filter(!is.na(over_under_line)) %>%
  mutate(result = if_else(score_sum > over_under_line, "Over Hit",
                          if_else(score_sum < over_under_line, "Under Hit", "Push"))) %>%
  group_by(result) %>%
  count() %>%
  mutate(prop = n / 8617)

linear_model <- lm(data = train_dat, score_sum ~ over_under_line)
quadratic_model <- lm(train_dat$score_sum ~ train_dat$over_under_line^2)
summary(linear_model)
summary(quadratic_model) # Wow, these are the exact same. Thats really weird...

log_model <- lm(data = train_dat, score_sum ~ log(over_under_line))
summary(log_model) # Even worse R^2

train_dat %>% filter(!is.na(bet_result_on_favorite)) %>%
  group_by(team_favorite_id) %>%
  summarise(mean_result_on_favorite = mean(bet_result_on_favorite)) %>% 
  arrange(desc(mean_result_on_favorite)) %>% print(n = 50)

train_dat %>% filter(!is.na(bet_result_on_favorite)) %>%
  mutate(team_underdog_id = if_else(team_favorite_id == team_home, team_away, if_else(
      team_favorite_id == team_away, team_home, "PICK"
    ))) %>%
  group_by(team_underdog_id) %>%
  summarise(mean_result_on_favorite = mean(bet_result_on_favorite)) %>% 
  arrange(mean_result_on_favorite) %>% print(n = 50)
